# Secrets Management Integration Example

## Overview

This example demonstrates secrets management with encrypted local storage and integration with external secret stores (HashiCorp Vault, AWS Secrets Manager).

## Features Demonstrated

1. **Local Encrypted Storage** - AES-256-GCM encryption for secrets
2. **Secret Caching** - ETS cache with TTL for performance
3. **Secret CRUD Operations** - Create, read, update, delete secrets
4. **Secret Rotation** - Automatic generation of new secrets
5. **Persistence** - Secrets survive restarts
6. **Failover** - Graceful degradation when external backends unavailable
7. **Key Management** - Automatic encryption key generation

## Prerequisites

- Erlang/OTP 25+
- rebar3 build system
- erlmcp_core application
- (Optional) HashiCorp Vault for remote backend demo
- (Optional) AWS Secrets Manager for AWS backend demo

## Running the Example

### Method 1: Using escript (Recommended)

```bash
# From the erlmcp root directory
escript examples/secrets_management/example.erl
```

### Method 2: Using rebar3 shell

```bash
# Start the Erlang shell
rebar3 shell

# Run the example
c("examples/secrets_management/example.erl").
example:main([]).
```

### Method 3: Interactive testing

```bash
# Start the Erlang shell
rebar3 shell

# Start applications
application:ensure_all_started(erlmcp_core).

# Start secrets manager
{ok, Pid} = erlmcp_secrets:start_link(#{
    backend => local_encrypted,
    storage_path => "priv/secrets/secrets.enc",
    encryption_key_path => "priv/secrets/master.key"
}).

# Store a secret
erlmcp_secrets:set_secret(<<"my_api_key">>, <<"sk-1234567890">>).

# Retrieve it
{ok, Key} = erlmcp_secrets:get_secret(<<"my_api_key">>).

# List all secrets
{ok, Keys} = erlmcp_secrets:list_secrets().

# Rotate a secret
erlmcp_secrets:rotate_secret(<<"my_api_key">>).

# Delete a secret
erlmcp_secrets:delete_secret(<<"my_api_key">>).
```

## Expected Output

```
=== Secrets Management Integration Example ===

Step 1: Starting erlmcp applications...
✓ Applications started

Step 2: Starting secrets manager (local_encrypted)...
✓ Secrets manager started: <0.123.0>

Step 3: Storing secrets...
✓ Stored: api_key=****
✓ Stored: db_password=****
✓ Stored: oauth_token=****

Step 4: Retrieving secrets (cache miss)...
✓ Retrieved api_key: sk-1...
✓ Retrieved db_password: Supe...

Step 5: Retrieving secrets (cache hit)...
✓ Retrieved api_key from cache: sk-1...

Step 6: Listing all secrets...
✓ Total secrets: 3
  - api_key
  - db_password
  - oauth_token

Step 7: Rotating a secret...
Old secret: sk-1...
✓ Rotated api_key
New secret: dGVzd...
✓ Verified new value: dGVzd...

Step 8: Verifying encrypted storage...
✓ Encrypted file: examples/secrets_management/secrets.enc (512 bytes)
✓ File mode: 100600

Step 9: Deleting a secret...
✓ Deleted: oauth_token
✓ Verified deletion (not_found)

Step 10: Demonstrating Vault failover to local...
✓ Vault config failed: {gun_open_failed,econnrefused}
✓ Staying on local_encrypted backend

Step 11: Testing persistence across restarts...
✓ Stopped secrets manager
✓ Restarted secrets manager
✓ Secrets persisted: 2

Step 12: Cleaning up...
✓ Cleanup complete

=== Example Complete ===
```

## Key Concepts

### Secret Backends

erlmcp supports multiple secret backends:

| Backend | Description | Encryption | Use Case |
|---------|-------------|------------|----------|
| **local_encrypted** | Disk file | AES-256-GCM | Development, small deployments |
| **vault** | HashiCorp Vault | TLS + Transit | Production, enterprise |
| **aws_secrets_manager** | AWS Secrets Manager | KMS | AWS deployments |

### Encryption Details

**local_encrypted backend:**
- Algorithm: AES-256-GCM (authenticated encryption)
- Key size: 256 bits (32 bytes)
- IV size: 96 bits (12 bytes, per GCM spec)
- Tag size: 128 bits (16 bytes, authentication tag)
- File format: `[IV (12 bytes)][Tag (16 bytes)][Ciphertext]`

**Key storage:**
- Location: `priv/secrets/master.key` (configurable)
- Format: Raw 32-byte binary
- Permissions: `0600` (read/write for owner only)
- Generation: `crypto:strong_rand_bytes(32)` on first run

### Secret Lifecycle

```
1. set_secret(Key, Value)
   - Encrypt with AES-256-GCM
   - Write to disk
   - Invalidate cache

2. get_secret(Key)
   - Check ETS cache (TTL: 5 min)
   - If cache miss: fetch from backend
   - Decrypt (local_encrypted)
   - Cache with TTL

3. rotate_secret(Key)
   - Generate new random secret (32 bytes)
   - Store in backend
   - Invalidate cache
   - Return new secret

4. delete_secret(Key)
   - Remove from backend
   - Invalidate cache

5. list_secrets()
   - Query backend
   - Return all keys (not values)
```

### Cache Performance

```
Operation          | Cache Hit | Cache Miss
-------------------|-----------|------------
get_secret         | ~1 μs     | ~10 ms (local)
set_secret         | N/A       | ~10 ms (local)
rotate_secret      | N/A       | ~10 ms (local)
```

## Advanced Usage

### HashiCorp Vault Integration

```erlang
% Configure Vault with token auth
erlmcp_secrets:configure_vault(#{
    url => <<"https://vault.example.com:8200">>,
    token => <<"s.vault-token">>,
    auth_method => token,
    mount => <<"secret">>,  % KV v2 engine
    namespace => <<"team-a">>,
    timeout => 5000
}).

% Configure Vault with AppRole auth
erlmcp_secrets:configure_vault(#{
    url => <<"https://vault.example.com:8200">>,
    auth_method => approle,
    role_id => <<"role-id">>,
    secret_id => <<"secret-id">>,
    mount => <<"secret">>
}).

% Now all operations use Vault
erlmcp_secrets:set_secret(<<"production/db">>, <<"password">>).
```

### AWS Secrets Manager Integration

```erlang
% Configure AWS (IAM role from metadata service)
erlmcp_secrets:configure_aws(#{
    enabled => true,
    region => <<"us-west-2">>,
    auth_method => iam_role
}).

% Configure AWS with access keys
erlmcp_secrets:configure_aws(#{
    enabled => true,
    region => <<"us-east-1">>,
    auth_method => access_key,
    access_key => <<"AKIAIOSFODNN7EXAMPLE">>,
    secret_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>
}).

% Use AWS Secrets Manager
erlmcp_secrets:set_secret(<<"prod/database">>, <<"SecretPassword123!">>).
```

### Custom Secret Encryption

```erlang
% Use custom encryption key (must be 32 bytes)
CustomKey = base64:decode("your-base64-encoded-256-bit-key"),

{ok, Pid} = erlmcp_secrets:start_link(#{
    backend => local_encrypted,
    storage_path => "custom_secrets.enc",
    encryption_key => CustomKey  % Override default key generation
}).
```

### Programmatic Key Rotation

```bash
# Rotate encryption key (requires re-encrypting all secrets)
# 1. Generate new key
NEW_KEY=$(openssl rand -base64 32)

# 2. Re-encrypt secrets file
# 3. Update master.key
# 4. Restart erlmcp_secrets
```

## Troubleshooting

### "Encryption key not found"

**Cause**: First time starting secrets manager.

**Solution**: Key auto-generated. Check:
```erlang
file:read_file("priv/secrets/master.key").
```

### "Failed to decrypt secrets"

**Cause**: Encrypted file corrupted or wrong encryption key.

**Solution**: Restore from backup:
```bash
cp secrets.enc.backup secrets.enc
cp master.key.backup master.key
```

### "Vault connection refused"

**Cause**: Vault not running or wrong URL.

**Solution**: Start Vault or verify URL:
```bash
# Check Vault health
curl https://vault.example.com:8200/v1/sys/health

# Start local Vault (dev mode)
vault server -dev
```

### "AWS credentials expired"

**Cause**: Temporary IAM role credentials expired.

**Solution**: Credentials auto-refresh. Check:
```erlang
% Verify metadata service reachable
httpc:request(get, {"http://169.254.169.254/latest/meta-data/iam/security-credentials/", []}, [], []).
```

## Security Best Practices

### Key Management

1. **Never commit `master.key` to version control**
   ```bash
   # Add .gitignore
   echo "priv/secrets/master.key" >> .gitignore
   ```

2. **Restrict key file permissions**
   ```bash
   chmod 600 priv/secrets/master.key
   ```

3. **Rotate encryption keys regularly**
   ```bash
   # Quarterly rotation recommended
   ```

### Secret Access

1. **Use least-privilege IAM roles for AWS**
2. **Enable Vault audit logging**
3. **Rotate secrets frequently** (especially API keys)
4. **Use short-lived tokens** (OAuth, JWT)

### Production Deployment

```erlang
% Production configuration
{erlmcp_core, [
    {secrets_backend, vault},
    {secrets_vault_config, #{
        url => <<"https://vault.prod.example.com:8200">>,
        auth_method => approle,
        role_id => {env, "VAULT_ROLE_ID"},
        secret_id => {env, "VAULT_SECRET_ID"},
        mount => <<"secret">>,
        namespace => <<"production">>
    }},
    {secrets_ttl_seconds => 60}  % 1-minute cache
]}
```

## File Security

### Encrypted File Format

```
Byte 0-11:    IV (Initialization Vector)
Byte 12-27:   GCM Tag (authentication tag)
Byte 28+:     AES-256-GCM ciphertext
```

### Verification

```bash
# Check file is encrypted (should be binary, not text)
file examples/secrets_management/secrets.enc
# Output: examples/secrets_management/secrets.enc: data

# Check permissions
ls -l examples/secrets_management/secrets.enc
# Output: -rw------- (600)

# Verify key permissions
ls -l examples/secrets_management/master.key
# Output: -rw------- (600)
```

## Integration with MCP Protocol

Secrets are used in MCP for:

- **Tool Authentication**: API keys for external services
- **Database Credentials**: Passwords for resource backends
- **OAuth Tokens**: Client credentials for OAuth flows
- **TLS Certificates**: Private keys for mTLS
- **Webhook Secrets**: HMAC keys for webhook verification

## Files

- `example.erl` - Main example script
- `README.md` - This file
- `secrets.enc` - Encrypted secrets storage (created at runtime)
- `master.key` - Encryption key (created at runtime)

## See Also

- [Secrets Module](../../../apps/erlmcp_core/src/erlmcp_secrets.erl)
- [Erlang Crypto Module](https://www.erlang.org/doc/man/crypto.html)
- [Vault API Documentation](https://developer.hashicorp.com/vault/api-docs)
- [AWS Secrets Manager API](https://docs.aws.amazon.com/secretsmanager/)
