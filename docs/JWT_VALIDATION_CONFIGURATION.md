# JWT Validation Configuration Guide

## Overview

The erlmcp_auth module now provides production-grade JWT validation using the `jose` library with full support for:

- **Cryptographic signature verification** (RS256, RS384, RS512, ES256, ES384, ES512, HS256, etc.)
- **Expiration checking** (exp claim) - MANDATORY
- **Not-before validation** (nbf claim) - Optional
- **Issuer validation** (iss claim) - Configurable
- **Audience validation** (aud claim) - Configurable
- **Subject validation** (sub claim) - MANDATORY
- **Key rotation support** via key ID (kid)
- **Default key fallback** for tokens without kid

## Security Features

### 1. No Authentication Bypass
- All JWT tokens MUST have valid signatures
- Expiration (exp) is REQUIRED and strictly enforced
- Subject (sub) is REQUIRED to identify the user
- Revoked tokens are rejected via ETS lookup

### 2. Base64url Decoding
- Uses jose library's `jose_jws:peek_protected/1` to properly handle base64url encoding
- No manual base64 decoding that could fail on URL-safe characters

### 3. Configuration-Based Validation
- Issuer and audience validation can be enforced via configuration
- Supports both single and multiple audience values (JWT arrays)

## Configuration

### Basic Configuration (No Issuer/Audience Enforcement)

```erlang
%% In sys.config or application environment
{erlmcp_core, [
    {auth, #{
        jwt => #{
            % Optional: default key when JWT has no 'kid' header
            default_key => <<"-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----">>
        },

        % JWT public keys by key ID (kid)
        jwt_keys => #{
            <<"key-2024-01">> => <<"-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----">>,
            <<"key-2024-02">> => <<"-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----">>
        }
    }}
]}.
```

### Advanced Configuration (Issuer + Audience Enforcement)

```erlang
{erlmcp_core, [
    {auth, #{
        jwt => #{
            % Enforce specific issuer (recommended for production)
            required_issuer => <<"https://auth.example.com">>,

            % Enforce specific audience (recommended for production)
            required_audience => <<"https://api.example.com">>,

            % Default key for JWTs without 'kid'
            default_key => <<"-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----">>
        },

        jwt_keys => #{
            <<"prod-key-1">> => <<"-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----">>,
            <<"prod-key-2">> => <<"-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----">>
        }
    }}
]}.
```

## JWT Token Format

### Minimal Valid JWT (Claims)

```json
{
  "exp": 1738368000,  // REQUIRED: Expiration timestamp (Unix epoch)
  "sub": "user123"    // REQUIRED: Subject (user identifier)
}
```

### Recommended JWT (Claims)

```json
{
  "exp": 1738368000,              // Expiration timestamp
  "nbf": 1738364400,              // Not-before timestamp (optional)
  "iss": "https://auth.example.com",  // Issuer (validated if configured)
  "aud": "https://api.example.com",   // Audience (validated if configured)
  "sub": "user123",               // Subject (user ID)
  "iat": 1738364400,              // Issued at (optional, not validated)
  "jti": "unique-token-id"        // JWT ID (optional, not validated)
}
```

### JWT Header (with Key ID)

```json
{
  "alg": "RS256",        // Algorithm (RS256, ES256, HS256, etc.)
  "typ": "JWT",
  "kid": "prod-key-1"    // Key ID for key rotation (optional)
}
```

## API Usage

### 1. Validate JWT Token

```erlang
%% Returns: {ok, Claims} | {error, Reason}
erlmcp_auth:validate_jwt(<<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...">>).
```

**Success Response:**
```erlang
{ok, #{
    <<"exp">> => 1738368000,
    <<"sub">> => <<"user123">>,
    <<"iss">> => <<"https://auth.example.com">>,
    <<"aud">> => <<"https://api.example.com">>
}}
```

**Error Responses:**
```erlang
{error, token_expired}           % Token expired (exp <= now)
{error, token_not_yet_valid}     % Not yet valid (nbf > now)
{error, invalid_signature}       % Signature verification failed
{error, missing_expiration}      % No 'exp' claim
{error, missing_subject}         % No 'sub' claim
{error, invalid_issuer}          % Issuer mismatch or invalid format
{error, invalid_audience}        % Audience mismatch or invalid format
{error, missing_audience}        % Required audience missing
{error, unknown_key_id}          % Key ID not found
{error, token_revoked}           % Token was revoked
{error, invalid_jwt_format}      % Malformed JWT
```

### 2. Authenticate with JWT

```erlang
%% Returns: {ok, SessionId} | {error, Reason}
erlmcp_auth:authenticate(jwt, #{
    token => <<"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...">>,
    ip_address => <<"192.168.1.100">>  % Optional: for rate limiting
}).
```

### 3. Rotate Public Key (Key Rotation)

```erlang
%% Add or update a public key
erlmcp_auth:rotate_public_key(
    <<"prod-key-3">>,  % Key ID
    <<"-----BEGIN PUBLIC KEY-----\n...\n-----END PUBLIC KEY-----">>
).
```

## Validation Flow

```
1. Check revoked tokens (ETS lookup)
   ↓
2. Peek JWT protected header (jose_jws:peek_protected/1)
   ↓
3. Extract key ID (kid) from header
   ↓
4. Lookup public key (ETS or default_key config)
   ↓
5. Verify signature (jose_jws:verify/2)
   ↓
6. Validate expiration (exp) - CRITICAL
   ↓
7. Validate not-before (nbf) - Optional
   ↓
8. Validate issuer (iss) - If configured
   ↓
9. Validate audience (aud) - If configured
   ↓
10. Validate subject (sub) - CRITICAL
    ↓
11. Return {ok, Claims}
```

## Supported Algorithms

The jose library supports all standard JWT algorithms:

- **RSA**: RS256, RS384, RS512
- **ECDSA**: ES256, ES384, ES512
- **HMAC**: HS256, HS384, HS512
- **EdDSA**: Ed25519, Ed448 (OTP 24+)

## Key Format

Public keys must be in PEM format:

```
-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA1234567890...
-----END PUBLIC KEY-----
```

For HMAC (HS256/HS384/HS512), use symmetric keys:

```
-----BEGIN SECRET KEY-----
base64_encoded_secret_key
-----END SECRET KEY-----
```

## Error Handling Best Practices

```erlang
%% Production-grade error handling
case erlmcp_auth:validate_jwt(Token) of
    {ok, Claims} ->
        UserId = maps:get(<<"sub">>, Claims),
        logger:info("JWT validated for user: ~p", [UserId]),
        {ok, UserId};

    {error, token_expired} ->
        logger:warning("JWT expired, token: ~p", [Token]),
        {error, <<"Token expired. Please login again.">>};

    {error, invalid_signature} ->
        logger:error("JWT signature invalid, token: ~p", [Token]),
        {error, <<"Invalid token.">>};

    {error, Reason} ->
        logger:error("JWT validation failed: ~p, token: ~p", [Reason, Token]),
        {error, <<"Authentication failed.">>}
end.
```

## Rate Limiting Integration

JWT validation automatically integrates with erlmcp_auth_rate_limiter:

```erlang
%% Enable rate limiting (default: true)
{erlmcp_core, [
    {auth, #{
        rate_limiter_enabled => true,
        jwt => #{...}
    }}
]}.
```

Rate limiting tracks:
- Failed authentication attempts per token
- IP-based blocking after repeated failures
- Success/failure recording for anomaly detection

## Testing

### Generate Test JWT (Using Python)

```python
import jwt
import time

payload = {
    "exp": int(time.time()) + 3600,  # 1 hour from now
    "sub": "test_user_123",
    "iss": "https://auth.example.com",
    "aud": "https://api.example.com",
    "nbf": int(time.time())
}

# RS256 with private key
with open('private_key.pem', 'r') as f:
    private_key = f.read()

token = jwt.encode(payload, private_key, algorithm='RS256', headers={"kid": "test-key-1"})
print(token)
```

### Generate Test Keys

```bash
# Generate RSA key pair
openssl genrsa -out private_key.pem 2048
openssl rsa -in private_key.pem -pubout -out public_key.pem

# Display public key for erlmcp config
cat public_key.pem
```

## Security Recommendations

1. **Always use RS256 or ES256** in production (asymmetric)
2. **Set required_issuer and required_audience** in production
3. **Use short expiration times** (exp) - 15-60 minutes recommended
4. **Implement token revocation** via erlmcp_auth:revoke_token/1
5. **Rotate keys regularly** using key IDs (kid)
6. **Enable rate limiting** to prevent brute force attacks
7. **Log all validation failures** for security monitoring

## Migration from Stub Implementation

The previous stub implementation has been completely replaced with:

1. **jose library integration** for cryptographic verification
2. **Configuration-based validation** for issuer/audience
3. **Proper base64url handling** via jose library
4. **Comprehensive error logging** at all validation steps
5. **No authentication bypass** - all paths require valid signatures

## Performance

- **ETS key lookup**: O(1) constant time
- **Signature verification**: ~1-2ms per token (RSA-2048)
- **Claims validation**: ~0.1ms per token
- **Total**: ~1-3ms per JWT validation

For high-throughput scenarios, consider:
- Caching validated JWTs (with short TTL matching exp)
- Using HMAC (HS256) if symmetric keys are acceptable
- Key preloading in ETS during initialization

---

**Implementation Date**: 2026-01-31
**Security Audit**: REQUIRED before production deployment
**Contact**: erlmcp-security@example.com
