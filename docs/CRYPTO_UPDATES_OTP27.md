# OTP 26-28 Crypto Updates for MCP Security

## Overview

This document describes the modern cryptographic features added to erlmcp using OTP 26-28 innovations, specifically for MCP (Model Context Protocol) security.

## OTP Version Features

### OTP 26
- **HMAC improvements**: Enhanced HMAC functions with better performance
- **Crypto module updates**: Improved key derivation and random number generation

### OTP 27
- **SHA-3 support**: New SHA-3 hash functions (sha3_224, sha3_256, sha3_384, sha3_512)
- **AEAD encryption**: `crypto:crypto_one_time_aead/7` for authenticated encryption
- **Enhanced PBKDF2**: Improved key derivation function

### OTP 28
- **OpenSSL 3.1.5**: Updated crypto engine with better performance
- **FIPS improvements**: Enhanced FIPS 140-2 compliance
- **Constant-time operations**: Better timing attack resistance

## Module: erlmcp_crypto

Core cryptographic utilities for MCP security.

### SHA-3 Hashing (OTP 27)

```erlang
%% SHA-3/256 hash (32-byte output)
Hash = erlmcp_crypto:sha3_256(<<"data">>),

%% SHA-3/512 hash (64-byte output)
Hash = erlmcp_crypto:sha3_512(<<"data">>).
```

**Use Cases:**
- Data integrity verification
- Checksum calculation
- Deterministic identifiers

### HMAC Signing (OTP 26)

```erlang
%% HMAC-SHA256 for request signing
Key = <<"signing_key">>,
Data = <<"request_data">>,
Signature = erlmcp_crypto:hmac_sha256(Key, Data).

%% HMAC-SHA3-256 for future-proof security
Signature = erlmcp_crypto:hmac_sha3_256(Key, Data).
```

**Use Cases:**
- Request/response signing
- API authentication
- Message authentication codes

### AEAD Encryption (OTP 27)

```erlang
%% AEAD encryption with AES-256-GCM
Plaintext = <<"sensitive data">>,
Key = crypto:strong_rand_bytes(32),  % 256-bit key
IV = erlmcp_crypto:generate_nonce(),  % 12-byte IV
AAD = <<"metadata">>,                 % Additional authenticated data

{ok, Ciphertext, Tag} = erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD),

%% Decrypt and verify
{ok, Decrypted} = erlmcp_crypto:decrypt_aead(Ciphertext, Tag, Key, IV, AAD).
```

**Use Cases:**
- Secure credential storage
- Encrypted communication
- Data-at-rest protection

**Security Properties:**
- **Confidentiality**: Ciphertext cannot be read without key
- **Integrity**: Tag verification detects tampering
- **Authenticity**: AAD binds encryption to context

### Token Generation

```erlang
%% Generate 32-byte secure token (256 bits)
Token = erlmcp_crypto:generate_token(),

%% Generate custom size token
Token16 = erlmcp_crypto:generate_token(16),
Token64 = erlmcp_crypto:generate_token(64).
```

**Use Cases:**
- Session IDs
- API keys
- Authentication tokens
- Nonces for replay protection

### Key Derivation (PBKDF2)

```erlang
%% Derive key from password
Password = <<"user_password">>,
Salt = crypto:strong_rand_bytes(16),
KeyLength = 32,  % 256-bit key
Iterations = 100000,  % OWASP recommendation

Key = erlmcp_crypto:derive_key(Password, Salt, KeyLength, Iterations).
```

**Use Cases:**
- Password-based encryption
- Master key derivation
- Secure token generation

### Constant-Time Comparison

```erlang
%% Prevent timing attacks on HMAC verification
Signature1 = erlmcp_crypto:hmac_sha256(Key, Data),
Signature2 = compute_signature(Data),

%% Use constant-time comparison
Valid = erlmcp_crypto:constant_time_compare(Signature1, Signature2).
```

**Security:**
- Prevents timing attacks on signature verification
- Essential for HMAC comparison

## Module: erlmcp_request_signer

Request/response signing for MCP protocol security.

### Request Signing

```erlang
%% Start request signer
{ok, _} = erlmcp_request_signer:start_link(#{
    signature_ttl => 300,  % 5 minutes
    cleanup_interval => 60000  % 1 minute
}),

%% Sign tool invocation request
Key = <<"hmac_signing_key">>,
Method = <<"tools/call">>,
Params = #{<<"name">> => <<"calculator">>, <<"arguments">> => #{<<"x">> => 5, <<"y">> => 3}},
RequestId = <<"req-123">>,

{ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId).
```

**Signature Data Structure:**
```erlang
#{
    method := <<"tools/call">>,
    params := #{...},
    timestamp := 1640995200,  % Unix timestamp
    nonce := <<12 bytes>>,    % Replay protection
    signature := base64      % HMAC-SHA256 signature
}
```

### Request Verification

```erlang
%% Verify request signature
CurrentTime = erlang:system_time(second),
Result = erlmcp_request_signer:verify_request(
    Key,
    SignatureData,
    RequestId,
    CurrentTime
),

%% Possible results:
%%   ok                    % Valid signature
%%   {error, signature_expired}     % Timestamp too old
%%   {error, nonce_reused}          % Replay attack detected
%%   {error, invalid_signature}     % Tampered data
```

**Security Features:**
- **Timestamp validation**: Prevents expired requests
- **Nonce tracking**: Prevents replay attacks
- **Constant-time comparison**: Prevents timing attacks
- **Canonical encoding**: Ensures consistent signature generation

### Response Signing

```erlang
%% Sign server response
Result = <<"{\"answer\": 8}">>,
{ok, SignatureData} = erlmcp_request_signer:sign_response(Key, Result, RequestId),

%% Verify response
Verified = erlmcp_request_signer:verify_response(Key, SignatureData, CurrentTime).
```

### Nonce Management

```erlang
%% Generate nonce
Nonce = erlmcp_request_signer:generate_nonce(),

%% Validate nonce (check for replay)
Valid = erlmcp_request_signer:validate_nonce(Nonce),

%% Mark nonce as used
ok = erlmcp_request_signer:mark_nonce_used(Nonce),

%% Cleanup expired nonces
ok = erlmcp_request_signer:cleanup_expired_nonces().
```

**Replay Protection:**
- ETS table tracks used nonces
- Nonces expire after TTL
- Periodic cleanup removes expired entries

## Module: erlmcp_secure_storage

Encrypted credential storage using AEAD.

### Encrypt Credential

```erlang
%% Start secure storage
{ok, _} = erlmcp_secure_storage:start_link(#{
    master_key => <<"master_encryption_key">>
}),

%% Encrypt API key
CredentialId = <<"openai_api_key">>,
Plaintext = <<"sk-1234567890abcdef">>,
{ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext).
```

**Encrypted Credential Structure:**
```erlang
#{
    ciphertext := <<...>>,     % Encrypted data
    iv := <<12 bytes>>,        % Initialization vector
    tag := <<16 bytes>>,       % Authentication tag
    key_id := <<...>>,         % Key version identifier
    key_version := 1,          % Key rotation number
    aad := <<...>>,            % Additional authenticated data
    created_at := 1640995200   % Creation timestamp
}
```

### Decrypt Credential

```erlang
%% Decrypt credential
{ok, Decrypted} = erlmcp_secure_storage:decrypt_credential(
    CredentialId,
    maps:get(ciphertext, Encrypted)
).
```

### Key Rotation

```erlang
%% Rotate to new encryption key
NewMasterKey = crypto:strong_rand_bytes(32),
{ok, NewKeyId} = erlmcp_secure_storage:rotate_key(NewMasterKey).

%% Re-encrypt all credentials with new key
{ok, Count} = erlmcp_secure_storage:reencrypt_all(NewMasterKey).
```

**Key Rotation Process:**
1. Generate new master key
2. Create new key entry with version number
3. Re-encrypt all credentials with new key
4. Old keys retained for decryption of legacy data
5. Audit trail via key versions

### Credential Metadata

```erlang
%% Get credential metadata (without decrypting)
{ok, Metadata} = erlmcp_secure_storage:get_credential_metadata(CredentialId),

%% List all credential IDs
{ok, Ids} = erlmcp_secure_storage:list_credentials(),

%% Delete credential
ok = erlmcp_secure_storage:delete_credential(CredentialId).
```

## Security Best Practices

### 1. Key Management

```erlang
%% Generate strong keys
MasterKey = crypto:strong_rand_bytes(32),  % 256-bit

%% Store keys securely (use erlmcp_secrets)
ok = erlmcp_secrets:set_secret(<<"master_key">>, MasterKey),

%% Rotate keys regularly
ok = rotate_master_key().
```

**Recommendations:**
- Use 256-bit keys (32 bytes) for AES-256
- Rotate master keys every 90 days
- Store keys in HashiCorp Vault or AWS Secrets Manager
- Never hardcode keys in source

### 2. Nonce/IV Generation

```erlang
%% Always use cryptographic random nonces
IV = erlmcp_crypto:generate_nonce(),  % 12 bytes for AES-GCM

%% Never reuse IV with same key
%% Never use predictable IV
```

**Best Practices:**
- Use `crypto:strong_rand_bytes/1` for nonces
- 12 bytes for AES-GCM (NIST recommendation)
- Never reuse nonce/key pair
- Generate fresh nonce for each encryption

### 3. Timestamp Validation

```erlang
%% Use appropriate TTL for signatures
SignatureTTL = 300,  % 5 minutes

%% Reject expired requests
case (CurrentTime - SignatureTime) =< SignatureTTL of
    true -> ok;
    false -> {error, signature_expired}
end.
```

**Recommendations:**
- 5 minutes for high-security operations
- 15 minutes for normal operations
- Clock synchronization required (NTP)

### 4. Replay Protection

```erlang
%% Always use nonces for requests
Nonce = erlmcp_request_signer:generate_nonce(),

%% Validate nonce hasn't been used
case erlmcp_request_signer:validate_nonce(Nonce) of
    true -> ok;
    false -> {error, nonce_reused}
end.
```

**Anti-Replay:**
- Track used nonces in ETS
- Expire nonces after TTL
- Clean up expired entries periodically

### 5. Constant-Time Operations

```erlang
%% Always use constant-time compare for HMAC
case erlmcp_crypto:constant_time_compare(Expected, Actual) of
    true -> ok;
    false -> {error, invalid_signature}
end.

%% NEVER use: =:= for HMAC comparison (timing attack)
```

**Why:**
- Normal comparison short-circuits on first mismatch
- Attackers can measure timing to guess valid signatures
- Constant-time comparison prevents this

## Integration with erlmcp

### 1. Tool Invocation Signing

```erlang
%% Client: Sign tool request
{ok, SignedReq} = erlmcp_request_signer:sign_request(
    SigningKey,
    <<"tools/call">>,
    Params,
    RequestId
),

%% Server: Verify tool request
case erlmcp_request_signer:verify_request(
    SigningKey,
    SignedReq,
    RequestId,
    CurrentTime
) of
    ok ->
        %% Execute tool
        execute_tool(Params);
    {error, Reason} ->
        {error, Reason}
end.
```

### 2. API Key Storage

```erlang
%% Store API key securely
{ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(
    <<"openai_api_key">>,
    <<"sk-...">>
),

%% Retrieve and decrypt
{ok, APIKey} = erlmcp_secure_storage:decrypt_credential(
    <<"openai_api_key">>,
    Ciphertext
).
```

### 3. Session Token Generation

```erlang
%% Generate secure session token
SessionToken = erlmcp_crypto:generate_token(),

%% Store token in session
Session#{token => SessionToken}.

%% Verify token format
case byte_size(SessionToken) of
    32 -> ok;  % 256 bits
    _ -> {error, invalid_token}
end.
```

### 4. Request/Response Logging

```erlang
%% Hash sensitive data for logging (not plaintext)
RequestHash = erlmcp_crypto:sha256(RequestPayload),

%% Log hash instead of plaintext
logger:info("Request: ~p", [RequestHash]).
```

## Migration Guide

### From Old Crypto to OTP 27+

#### 1. Replace SHA-2 with SHA-3 (Optional)

```erlang
%% Old (OTP < 27)
Hash = crypto:hash(sha256, Data),

%% New (OTP 27+)
Hash = erlmcp_crypto:sha3_256(Data).
```

**Benefits:**
- SHA-3 is more secure against length-extension attacks
- Better performance on modern hardware
- Future-proofs your application

#### 2. Use AEAD Instead of CBC

```erlang
%% Old (CBC mode - no authentication)
IV = crypto:strong_rand_bytes(16),
Ciphertext = crypto:crypto_one_time(aes_256_cbc, Key, IV, Plaintext, true),

%% New (AEAD mode - authenticated encryption)
IV = erlmcp_crypto:generate_nonce(),
{ok, Ciphertext, Tag} = erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD).
```

**Benefits:**
- Authentication built-in
- Tamper detection via tag
- No need for separate HMAC

#### 3. Add Request Signing

```erlang
%% Old: No request signing
execute_tool(Params),

%% New: Sign and verify requests
{ok, Signed} = erlmcp_request_signer:sign_request(Key, Method, Params, ReqId),
ok = verify_request_signature(Signed),
execute_tool(Params).
```

**Benefits:**
- Detect request tampering
- Prevent replay attacks
- Audit trail via signatures

## Performance Considerations

### Crypto Operations Cost

| Operation | Time | Notes |
|-----------|------|-------|
| SHA-3/256 | ~1μs | 32-byte input |
| HMAC-SHA256 | ~2μs | 32-byte key, 32-byte data |
| AEAD encrypt | ~10μs | 1KB plaintext |
| PBKDF2 (100K iter) | ~100ms | Password derivation |

### Optimization Tips

1. **Cache derived keys**: Don't re-derive on every operation
2. **Use SHA-256 for non-critical hashing**: Faster than SHA-3
3. **Batch crypto operations**: Parallelize independent encryptions
4. **Reuse IV pools**: Pre-generate nonces for high-throughput scenarios

### Memory Usage

| Operation | Memory |
|-----------|--------|
| SHA-3/256 | ~64 bytes |
| HMAC-SHA256 | ~128 bytes |
| AEAD encrypt | ~2x plaintext size |

## Testing

### Unit Tests

```bash
%% Run crypto tests
rebar3 eunit --module=erlmcp_crypto_tests

%% Run request signer tests
rebar3 eunit --module=erlmcp_request_signer_tests

%% Run secure storage tests
rebar3 eunit --module=erlmcp_secure_storage_tests
```

### Coverage Requirements

All crypto modules must maintain **≥80% code coverage** (Chicago School TDD).

### Test Vectors

Use NIST test vectors for:
- SHA-3 hashing
- HMAC algorithms
- AES-GCM encryption

Example test vectors in `erlmcp_crypto_tests.erl`.

## Security Audit Checklist

- [ ] All crypto operations use OTP 27+ features
- [ ] Keys are 256 bits (32 bytes) minimum
- [ ] Nonces are cryptographically random
- [ ] Constant-time comparison for HMAC
- [ ] Timestamp validation on signatures
- [ ] Nonce replay protection enabled
- [ ] AEAD used for all encryption
- [ ] Key rotation mechanism in place
- [ ] No hardcoded keys in source
- [ ] Secrets stored in Vault/AWS SM
- [ ] Test coverage ≥80%
- [ ] Dialyzer warnings = 0
- [ ] No timing attack vulnerabilities

## Troubleshooting

### Common Issues

#### 1. `function_clause` in AEAD

**Cause**: OTP < 27 doesn't support `crypto:crypto_one_time_aead/7`

**Fix**: Check OTP version and provide fallback
```erlang
case erlang:system_info(otp_release) >= 27 of
    true -> use_aead();
    false -> use_legacy_cbc()
end.
```

#### 2. Nonce Replay Detected

**Cause**: Same nonce used twice with same key

**Fix**: Always generate fresh nonce
```erlang
IV = erlmcp_crypto:generate_nonce(),  % New every time
```

#### 3. Authentication Failed

**Cause**: Wrong AAD or tampered data

**Fix**: Ensure AAD matches on encrypt/decrypt
```erlang
%% Same AAD on both sides
AAD = build_context(CredentialId),
{ok, Encrypted} = encrypt(Plaintext, Key, IV, AAD),
{ok, Decrypted} = decrypt(Ciphertext, Tag, Key, IV, AAD).
```

## References

- [Erlang Crypto Module](https://www.erlang.org/doc/man/crypto.html)
- [NIST SHA-3 Standard](https://csrc.nist.gov/projects/hash-functions/sha-3)
- [AES-GCM (NIST SP 800-38D)](https://csrc.nist.gov/publications/detail/sp/800-38d/final)
- [OWASP Key Derivation](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)
- [RFC 4868 - HMAC](https://www.rfc-editor.org/rfc/rfc4868)

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0.0 | 2025-01-01 | Initial implementation with OTP 27 features |

## Contributing

When adding crypto features:

1. **Security review**: All crypto changes require security review
2. **Test vectors**: Include NIST test vectors for validation
3. **Documentation**: Update this file with new patterns
4. **OTP version**: Document minimum OTP version required
5. **Performance**: Benchmark critical crypto operations

## License

Copyright (c) 2025 erlmcp project. See LICENSE file for details.
