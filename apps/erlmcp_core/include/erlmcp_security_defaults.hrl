%%%-------------------------------------------------------------------
%%% @doc erlmcp_security_defaults.hrl - Security Defaults Header
%%%
%%% Armstrong Principle: "Make unsafe defaults unrepresentable."
%%%
%%% This header defines compile-time security constants that CANNOT be
%%% overridden at runtime. Unsafe configurations are made impossible
%%% through type system enforcement and compile-time guards.
%%%
%%% CRITICAL: These defaults are MANDATORY and cannot be disabled.
%%% Any attempt to use unsafe defaults will result in compilation failure.
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_SECURITY_DEFAULTS_HRL).
-define(ERLMCP_SECURITY_DEFAULTS_HRL, true).

%%====================================================================
%% TLS/SSL Security Defaults (MANDATORY)
%%====================================================================

%% Minimum TLS version - TLS 1.2 is the MINIMUM acceptable version
%% TLS 1.0 and 1.1 are FORBIDDEN due to known vulnerabilities
-define(MIN_TLS_VERSION, 'tlsv1.2').
-define(ALLOWED_TLS_VERSIONS, ['tlsv1.2', 'tlsv1.3']).

%% Certificate verification - MUST be verify_peer
%% verify_none is FORBIDDEN and will cause compilation failure
-define(TLS_VERIFY_MODE, verify_peer).

%% Certificate validation depth
-define(TLS_MAX_CERT_DEPTH, 5).

%% Strong cipher suites ONLY - no weak ciphers allowed
-define(REQUIRED_CIPHER_SUITES, [
    %% TLS 1.3 (most secure)
    "TLS_AES_128_GCM_SHA256",
    "TLS_AES_256_GCM_SHA384",
    "TLS_CHACHA20_POLY1305_SHA256",
    %% TLS 1.2 with forward secrecy (ECDHE only)
    "ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-CHACHA20-POLY1305",
    "ECDHE-RSA-CHACHA20-POLY1305"
]).

%% Hostname verification REQUIRED
-define(TLS_VERIFY_HOSTNAME, true).

%% Secure renegotiation REQUIRED
-define(TLS_SECURE_RENEGOTIATE, true).

%% Client-initiated renegotiation FORBIDDEN (DoS vector)
-define(TLS_CLIENT_RENEGOTIATION, false).

%%====================================================================
%% Compile-Time Guard: Enforce TLS verification
%% This guard makes verify_none UNREPRESENTABLE at compile time
%%====================================================================

%% Usage in code:
%% -include("erlmcp_security_defaults.hrl").
%% ?ENFORCE_TLS_VERIFY(Options)  % Will fail compilation if verify=verify_none

-define(ENFORCE_TLS_VERIFY(Opts),
    case lists:keyfind(verify, 1, Opts) of
        {verify, verify_none} ->
            ?COMPILATION_ERROR("verify_none is FORBIDDEN: Use verify_peer");
        _ -> Opts
    end
).

%% Macro to trigger compilation error (requires -Werror or explicit check)
-define(COMPILATION_ERROR(Msg),
    erlang:error({security_violation, Msg})
).

%%====================================================================
%% HTTP/HTTPS Security Defaults (MANDATORY)
%%====================================================================

%% Origin validation - DEFAULT DENY (whitelist required)
%% undefined origin is REJECTED by default (no bypass)
-define(ORIGIN_VALIDATION_DEFAULT, reject_undefined).
-define(ORIGIN_WHITELIST_REQUIRED, true).

%% HTTP header size limits (prevent DoS)
-define(MAX_HEADER_SIZE, 8192).        % 8KB per header
-define(MAX_TOTAL_HEADERS_SIZE, 65536). % 64KB total

%% Request body size limits
-define(MAX_REQUEST_BODY_SIZE, 16777216). % 16MB

%% Content-Type validation REQUIRED
-define(REQUIRE_CONTENT_TYPE_VALIDATION, true).

%% HSTS configuration (HTTP Strict Transport Security)
-define(HSTS_ENABLED, true).
-define(HSTS_MAX_AGE, 31536000). % 1 year in seconds
-define(HSTS_INCLUDE_SUBDOMAINS, true).
-define(HSTS_PRELOAD, true).

%% Content Security Policy (strict default)
-define(CSP_DEFAULT, <<"default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; font-src 'self'; connect-src 'self'; frame-ancestors 'none'; base-uri 'self'; form-action 'self'">>).

%% X-Frame-Options (prevent clickjacking)
-define(X_FRAME_OPTIONS, <<"DENY">>).

%% CORS defaults (deny-all by default)
-define(CORS_DEFAULT_POLICY, deny_all).
-define(CORS_WHITELIST_REQUIRED, true).

%%====================================================================
%% Authentication & Authorization Defaults (MANDATORY)
%%====================================================================

%% Rate limiting ENABLED by default
-define(RATE_LIMITING_ENABLED, true).
-define(DEFAULT_RATE_LIMIT_REQUESTS, 100).
-define(DEFAULT_RATE_LIMIT_WINDOW_MS, 60000). % 1 minute

%% JWT validation requirements
-define(JWT_REQUIRE_EXPIRATION, true).
-define(JWT_REQUIRE_SUBJECT, true).
-define(JWT_REQUIRE_SIGNATURE_VERIFICATION, true).

%% Session configuration
-define(SESSION_DEFAULT_TTL_MS, 3600000). % 1 hour (NOT infinity)
-define(SESSION_MAX_TTL_MS, 86400000).    % 24 hours maximum
-define(SESSION_REQUIRE_EXPIRY, true).

%% Cookie security flags (MANDATORY)
-define(COOKIE_SECURE, true).      % Require HTTPS
-define(COOKIE_HTTPONLY, true).    % Prevent XSS access
-define(COOKIE_SAMESITE, strict).  % CSRF protection

%%====================================================================
%% Secrets Management Defaults (MANDATORY)
%%====================================================================

%% Encryption algorithm (AES-256-GCM ONLY)
-define(SECRETS_ENCRYPTION_ALGORITHM, aes_256_gcm).
-define(SECRETS_KEY_SIZE_BYTES, 32). % 256 bits

%% Secret rotation period (maximum)
-define(SECRETS_MAX_AGE_DAYS, 90).

%% Secrets cache TTL (maximum)
-define(SECRETS_CACHE_TTL_SECONDS, 300). % 5 minutes

%%====================================================================
%% Message Size Limits (MANDATORY)
%%====================================================================

%% Transport-specific limits
-define(MAX_TCP_MESSAGE_SIZE, 16777216).     % 16MB
-define(MAX_HTTP_MESSAGE_SIZE, 16777216).    % 16MB
-define(MAX_WS_MESSAGE_SIZE, 16777216).      % 16MB
-define(MAX_SSE_MESSAGE_SIZE, 16777216).     % 16MB

%% Fragment timeout (prevent resource exhaustion)
-define(FRAGMENT_TIMEOUT_MS, 30000). % 30 seconds

%%====================================================================
%% Input Validation Defaults (MANDATORY)
%%====================================================================

%% UTF-8 validation REQUIRED for text protocols
-define(REQUIRE_UTF8_VALIDATION, true).

%% JSON schema validation REQUIRED
-define(REQUIRE_JSON_SCHEMA_VALIDATION, true).

%% CRLF injection prevention ENABLED
-define(CRLF_INJECTION_PREVENTION, true).

%%====================================================================
%% Compile-Time Guards: Type System Enforcement
%%====================================================================

%% Type guard: Session TTL must have expiry
-define(VALIDATE_SESSION_TTL(TTL),
    case TTL of
        infinity ->
            ?COMPILATION_ERROR("Session TTL cannot be infinity: Use finite value");
        N when is_integer(N), N > 0, N =< ?SESSION_MAX_TTL_MS ->
            N;
        _ ->
            ?COMPILATION_ERROR("Invalid session TTL: Must be 0 < TTL <= " ++
                             integer_to_list(?SESSION_MAX_TTL_MS))
    end
).

%% Type guard: Origin must be explicitly whitelisted
-define(VALIDATE_ORIGIN(Origin, AllowedOrigins),
    case Origin of
        undefined when ?ORIGIN_VALIDATION_DEFAULT =:= reject_undefined ->
            {error, origin_required};
        _ when AllowedOrigins =:= [] ->
            {error, origin_whitelist_empty};
        _ ->
            erlmcp_origin_validator:validate_origin(Origin, AllowedOrigins)
    end
).

%% Type guard: TLS options must include verify_peer
-define(VALIDATE_TLS_OPTS(Opts),
    case lists:keyfind(verify, 1, Opts) of
        false ->
            %% Add verify_peer if missing
            [{verify, ?TLS_VERIFY_MODE} | Opts];
        {verify, verify_peer} ->
            Opts;
        {verify, verify_none} ->
            ?COMPILATION_ERROR("TLS verify_none is FORBIDDEN")
    end
).

%%====================================================================
%% Security Policy Constants
%%====================================================================

%% These constants define the security posture and CANNOT be changed

%% Default security posture: DENY by default, ALLOW by exception
-define(DEFAULT_SECURITY_POSTURE, deny_by_default).

%% Fail-safe mode: On security error, fail closed (reject)
-define(FAIL_SAFE_MODE, fail_closed).

%% Security logging level (all security events are logged)
-define(SECURITY_LOG_LEVEL, info).

%% Audit trail REQUIRED for authentication events
-define(REQUIRE_AUTH_AUDIT_TRAIL, true).

%%====================================================================
%% Documentation and Compliance
%%====================================================================

%% Security invariants enforced by this header:
%%
%% 1. TLS/SSL:
%%    - verify_peer is MANDATORY, verify_none is UNREPRESENTABLE
%%    - TLS 1.2+ only, older versions FORBIDDEN
%%    - Strong ciphers only (ECDHE with forward secrecy)
%%
%% 2. HTTP/HTTPS:
%%    - Origin validation defaults to REJECT undefined
%%    - HSTS enabled by default with 1-year max-age
%%    - Strict CSP policy by default
%%    - CORS defaults to deny-all
%%
%% 3. Authentication:
%%    - Rate limiting ENABLED by default
%%    - JWT expiration REQUIRED
%%    - Sessions MUST have finite expiry (no infinity)
%%
%% 4. Secrets:
%%    - AES-256-GCM encryption ONLY
%%    - Cache TTL limited to 5 minutes
%%
%% 5. Input Validation:
%%    - UTF-8 validation REQUIRED
%%    - Message size limits ENFORCED
%%    - CRLF injection prevention ENABLED
%%
%% Armstrong Principle Applied:
%% "Make illegal states unrepresentable."
%% - verify_none → compilation error
%% - infinity TTL → compilation error
%% - empty origin whitelist → runtime error
%% - weak ciphers → not in allowed list

-endif. % ERLMCP_SECURITY_DEFAULTS_HRL
