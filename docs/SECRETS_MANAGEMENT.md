# Secrets Management - erlmcp

## Overview

erlmcp provides a comprehensive secrets management system that integrates with HashiCorp Vault and AWS Secrets Manager, with encrypted local storage as a fallback. Secrets are cached in-memory with TTL for performance, and all sensitive data is encrypted at rest using AES-256-GCM.

### Architecture

```
┌──────────────────────────────────────────────────────────────┐
│                     erlmcp_secrets                            │
│                      (gen_server)                             │
└─────┬───────────────────────────────────┬────────────────────┘
      │                                   │
      │ caches (TTL)                       │ stores
      ▼                                   ▼
┌─────────────┐                   ┌──────────────────────────┐
│ ETS Cache   │                   │   Backend (pluggable)    │
│ (5 min TTL) │                   │                          │
└─────────────┘                   ├──────────────────────────┤
                                  │ HashiCorp Vault         │
                                  │ AWS Secrets Manager     │
                                  │ Local Encrypted (AES)   │
                                  └──────────────────────────┘
```

### Key Features

- **Multiple Backends**: Vault, AWS Secrets Manager, local encrypted storage
- **Intelligent Caching**: ETS cache with configurable TTL (default: 5 minutes)
- **Encryption at Rest**: AES-256-GCM encryption for local storage
- **Secret Rotation**: Automatic rotation with configurable policies
- **Fallback Support**: Graceful degradation to local storage
- **Audit Logging**: All secret access logged for compliance

## HashiCorp Vault Integration

### Prerequisites

```bash
# Install Vault (optional, for development)
brew tap hashicorp/tap
brew install vault

# Start Vault dev server (development only)
vault server -dev
```

### Configuration

#### Token Authentication

```erlang
%% config/sys.config
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        address => "http://localhost:8200",
        token => "s.1234567890abcdef",  % Vault token
        engine => "kv",                 % KV secrets engine version
        mount => "secret"               % Mount point
    }},
    {ttl_seconds => 300},               % Cache TTL: 5 minutes
    {storage_path => "priv/secrets/secrets.enc"}
]}.
```

**Programmatic setup:**
```erlang
%% Start secrets manager with Vault backend
{ok, Pid} = erlmcp_secrets:start_link(#{
    backend => vault,
    backend_config => #{
        address => "http://localhost:8200",
        token => "s.1234567890abcdef",
        engine => "kv",
        mount => "secret"
    },
    ttl_seconds => 300
}).

%% Configure Vault after start
ok = erlmcp_secrets:configure_vault(#{
    address => "https://vault.example.com:8200",
    token => "s.abcdef1234567890"
}).
```

#### AppRole Authentication (Recommended for Production)

```erlang
%% config/sys.config
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        address => "https://vault.example.com:8200",
        auth_method => approle,
        role_id => "12345678-1234-1234-1234-123456789012",
        secret_id => "87654321-4321-4321-4321-210987654321",
        engine => "kv",
        mount => "secret"
    }}
]}.
```

#### Kubernetes Authentication

```erlang
%% config/sys.config
{erlmcp_secrets, [
    {backend, vault},
    {backend_config => #{
        address => "https://vault.example.com:8200",
        auth_method => kubernetes,
        role => "erlmcp-role",
        jwt_path => "/var/run/secrets/kubernetes.io/serviceaccount/token",
        engine => "kv",
        mount => "secret"
    }}
]}.
```

### Usage Examples

#### Store and Retrieve Secrets

```erlang
%% Store secret in Vault
ok = erlmcp_secrets:set_secret(<<"database/password">>, <<"SuperSecret123">>).

%% Retrieve secret (with caching)
{ok, Password} = erlmcp_secrets:get_secret(<<"database/password">>).

%% Retrieve from Vault (secret path: secret/database/password)
%% Value: "SuperSecret123"
```

#### Secret Rotation

```erlang
%% Rotate secret (generates new random value)
{ok, NewPassword} = erlmcp_secrets:rotate_secret(<<"database/password">>).

%% Returns: {ok, <<"base64-encoded-32-byte-random-value">>}
%% Secret automatically updated in Vault
%% Cache invalidated immediately
```

#### List Secrets

```erlang
%% List all secret keys
{ok, Keys} = erlmcp_secrets:list_secrets().

%% Returns: {ok, [<<"database/password">>, <<"api/key">>, ...]}
```

#### Delete Secret

```erlang
%% Delete secret from Vault
ok = erlmcp_secrets:delete_secret(<<"database/password">>).
```

## AWS Secrets Manager Integration

### Prerequisites

```bash
# Install AWS CLI (for local testing)
brew install awscli

# Configure credentials
aws configure
# AWS Access Key ID: XXXXXX
# AWS Secret Access Key: XXXXXX
# Default region: us-east-1
```

### Configuration

#### IAM Authentication

```erlang
%% config/sys.config
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => "us-east-1",
        access_key_id => "AKIAIOSFODNN7EXAMPLE",
        secret_access_key => "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
        prefix => "erlmcp/"  % Optional: prefix for all secrets
    }},
    {ttl_seconds => 300}
]}.
```

**Programmatic setup:**
```erlang
%% Start with AWS backend
{ok, Pid} = erlmcp_secrets:start_link(#{
    backend => aws_secrets_manager,
    backend_config => #{
        region => "us-east-1",
        access_key_id => "AKIAIOSFODNN7EXAMPLE",
        secret_access_key => "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
        prefix => "erlmcp/"
    },
    ttl_seconds => 300
}).

%% Or configure after start
ok = erlmcp_secrets:configure_aws(#{
    region => "us-west-2",
    access_key_id => "...",
    secret_access_key => "..."
}).
```

#### IAM Role (EC2/ECS/Lambda)

```erlang
%% config/sys.config
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config => #{
        region => "us-east-1",
        use_iam_role => true  % Automatically use EC2/ECS IAM role
    }}
]}.
```

### Usage Examples

```erlang
%% Store secret (creates secret in AWS Secrets Manager)
ok = erlmcp_secrets:set_secret(<<"erlmcp/database/password">>, <<"SuperSecret123">>).

%% Retrieve secret (with caching)
{ok, Password} = erlmcp_secrets:get_secret(<<"erlmcp/database/password">>).

%% Rotate secret (AWS handles automatic rotation)
{ok, NewPassword} = erlmcp_secrets:rotate_secret(<<"erlmcp/database/password">>).

%% List secrets
{ok, Keys} = erlmcp_secrets:list_secrets().

%% Delete secret
ok = erlmcp_secrets:delete_secret(<<"erlmcp/database/password">>).
```

## Local Encrypted Storage (Fallback)

### Overview

When Vault or AWS Secrets Manager are unavailable, erlmcp falls back to local encrypted storage using AES-256-GCM encryption. Secrets are encrypted with a key derived from a master password or environment variable.

### Configuration

```erlang
%% config/sys.config
{erlmcp_secrets, [
    {backend, local_encrypted},
    {encryption_key => "your-master-password-here"},  % OR use {env_var, "ERLMCP_SECRET_KEY"}
    {storage_path => "priv/secrets/secrets.enc"},
    {ttl_seconds => 300}
]}.
```

**Environment variable (recommended):**
```erlang
%% config/sys.config
{erlmcp_secrets, [
    {backend, local_encrypted},
    {encryption_key => {env_var, "ERLMCP_SECRET_KEY"}},
    {storage_path => "priv/secrets/secrets.enc"}
]}.
```

**Shell:**
```bash
# Set encryption key in environment
export ERLMCP_SECRET_KEY="$(openssl rand -base64 32)"

# Start Erlang node
rebar3 shell
```

### Usage Examples

```erlang
%% Store secret (encrypted locally)
ok = erlmcp_secrets:set_secret(<<"api/key">>, <<"sk-1234567890">>).

%% Retrieve secret (decrypted on-the-fly)
{ok, ApiKey} = erlmcp_secrets:get_secret(<<"api/key">>).

%% Rotate secret
{ok, NewApiKey} = erlmcp_secrets:rotate_secret(<<"api/key">>).

%% List all secret keys
{ok, Keys} = erlmcp_secrets:list_secrets().

%% Delete secret
ok = erlmcp_secrets:delete_secret(<<"api/key">>).
```

## Security Best Practices

### 1. Never Store Secrets in Code

```erlang
%% ❌ BAD: Hardcoded secrets
ApiKey = "sk-1234567890abcdef".

%% ✅ GOOD: Retrieve from secrets manager
{ok, ApiKey} = erlmcp_secrets:get_secret(<<"api/key">>).
```

### 2. Use Least Privilege Access

**Vault:**
```erlang
%% Policy for erlmcp (Vault policy.hcl)
path "secret/data/erlmcp/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}
```

**AWS:**
```json
// IAM policy for erlmcp
{
  "Version": "2012-10-17",
  "Statement": [{
    "Effect": "Allow",
    "Action": [
      "secretsmanager:GetSecretValue",
      "secretsmanager:CreateSecret",
      "secretsmanager:PutSecretValue",
      "secretsmanager:DeleteSecret"
    ],
    "Resource": "arn:aws:secretsmanager:us-east-1:123456789012:secret:erlmcp/*"
  }]
}
```

### 3. Enable Audit Logging

```erlang
%% Enable logging for all secret access
logger:set_log_level(info),

%% All secret accesses are logged:
%% - get_secret: {info, "Secret retrieved: ~p", [Key]}
%% - set_secret: {info, "Secret stored: ~p", [Key]}
%% - delete_secret: {info, "Secret deleted: ~p", [Key]}
%% - rotate_secret: {info, "Secret rotated: ~p", [Key]}
```

### 4. Rotate Secrets Regularly

```erlang
%% Schedule periodic rotation (using erlcron or timer)
rotate_database_password() ->
    {ok, NewPassword} = erlmcp_secrets:rotate_secret(<<"database/password">>),
    %% Update database with new password
    update_db_password(NewPassword).

%% Schedule every 90 days
erlcron:add_cron_job({rotate_database_password, {
  0, 0, "*/90 * * * *"  % Every 90 days at midnight
}}).
```

### 5. Use Separate Secrets per Environment

```erlang
%% Development
{erlmcp_secrets, [
    {backend, local_encrypted},
    {storage_path => "priv/secrets/dev.enc"}
]}.

%% Production
{erlmcp_secrets, [
    {backend, vault},
    {backend_config => #{
        address => "https://vault-prod.example.com"
    }}
]}.
```

### 6. Encrypt Secrets in Transit

```erlang
%% Always use HTTPS for Vault/AWS
%% ❌ BAD
backend_config => #{address => "http://vault.example.com"}

%% ✅ GOOD
backend_config => #{address => "https://vault.example.com"}
```

## API Reference

### Start and Configure

```erlang
%% Start with default config (local_encrypted)
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% Start with custom config
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    %% Config: #{
    ///   backend => vault | aws_secrets_manager | local_encrypted,
    ///   backend_config => map(),
    ///   ttl_seconds => pos_integer(),
    ///   storage_path => file:filename()
    /// }

%% Configure Vault backend
-spec configure_vault(map()) -> ok.
configure_vault(Config) ->
    %% Config: #{
    ///   address => string(),
    ///   token => binary(),
    ///   auth_method => token | approle | kubernetes,
    ///   role_id => binary(),  % for approle
    ///   secret_id => binary(), % for approle
    ///   engine => binary(),    % "kv" or "kv-v2"
    ///   mount => binary()     % "secret" or custom mount
    /// }

%% Configure AWS backend
-spec configure_aws(map()) -> ok.
configure_aws(Config) ->
    %% Config: #{
    ///   region => string(),
    ///   access_key_id => binary(),
    ///   secret_access_key => binary(),
    ///   use_iam_role => boolean(),
    ///   prefix => binary()
    /// }

%% Stop secrets manager
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).
```

### Secret Operations

```erlang
%% Get secret by key
-spec get_secret(secret_key()) -> {ok, secret_value()} | {error, term()}.
get_secret(Key) ->
    %% Checks cache first (TTL)
    %% Fetches from backend on cache miss
    %% Caches result with TTL

%% Set secret
-spec set_secret(secret_key(), secret_value()) -> ok | {error, term()}.
set_secret(Key, Value) ->
    %% Stores in backend
    %% Invalidates cache

%% Delete secret
-spec delete_secret(secret_key()) -> ok | {error, term()}.
delete_secret(Key) ->
    %% Deletes from backend
    %% Invalidates cache

%% Rotate secret (generate new random value)
-spec rotate_secret(secret_key()) -> {ok, secret_value()} | {error, term()}.
rotate_secret(Key) ->
    %% Generates 32-byte random value (base64-encoded)
    %% Updates backend
    %% Invalidates cache

%% List all secret keys
-spec list_secrets() -> {ok, [secret_key()]} | {error, term()}.
list_secrets() ->
    %% Returns list of all secret keys
```

## Configuration Examples

### Development Environment

```erlang
%% config/dev.config
{erlmcp_secrets, [
    {backend, local_encrypted},
    {encryption_key => {env_var, "ERLMCP_SECRET_KEY"}},
    {storage_path => "priv/secrets/dev.enc"},
    {ttl_seconds => 60}  % Shorter TTL for development
]}.
```

### Production Environment (Vault)

```erlang
%% config/prod.config
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        address => "https://vault.prod.example.com:8200",
        auth_method => approle,
        role_id => {env_var, "VAULT_ROLE_ID"},
        secret_id => {env_var, "VAULT_SECRET_ID"},
        engine => "kv-v2",
        mount => "secret"
    }},
    {ttl_seconds => 600},  % Longer TTL for production
    {storage_path => "priv/secrets/fallback.enc"}  % Fallback storage
]}.
```

### Production Environment (AWS)

```erlang
%% config/prod_aws.config
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => "us-east-1",
        use_iam_role => true,
        prefix => "erlmcp/prod/"
    }},
    {ttl_seconds => 600}
]}.
```

## Troubleshooting

### Vault Connection Issues

```erlang
%% Symptom: {error, connection refused}
%% Solution: Check Vault address and network connectivity
{ok, _} = erlmcp_secrets:configure_vault(#{
    address => "http://localhost:8200",  % Verify address
    token => "..."
}).

%% Test Vault connectivity
httpc:request(get, {"http://localhost:8200/v1/sys/health", []}, [], []).
```

### AWS Authentication Issues

```erlang
%% Symptom: {error, unauthorized}
%% Solution: Verify IAM credentials and permissions
aws_cli:configure("us-east-1", "ACCESS_KEY", "SECRET_KEY").

%% Test AWS access
aws_cli:secretsmanager("list-secrets").
```

### Local Encryption Key Issues

```erlang
%% Symptom: {error, invalid_encryption_key}
%% Solution: Ensure encryption key is set correctly
%% Set environment variable
os:putenv("ERLMCP_SECRET_KEY", "base64-encoded-key").

%% Or use hardcoded key (NOT RECOMMENDED for production)
{ok, Pid} = erlmcp_secrets:start_link(#{
    encryption_key => "your-master-password"
}).
```

### Cache Issues

```erlang
%% Symptom: Stale secret returned
%% Solution: Invalidate cache manually
ets:delete(secrets_cache, <<"secret/key">>).

%% Or restart secrets manager
erlmcp_secrets:stop(),
{ok, Pid} = erlmcp_secrets:start_link(Config).
```

## References

- [HashiCorp Vault Documentation](https://www.vaultproject.io/docs)
- [AWS Secrets Manager Documentation](https://docs.aws.amazon.com/secretsmanager/)
- [Erlang crypto Module](https://erlang.org/doc/man/crypto.html)
- `apps/erlmcp_core/src/erlmcp_secrets.erl`
- `apps/erlmcp_core/test/erlmcp_secrets_vault_tests.erl`
- `apps/erlmcp_core/test/erlmcp_secrets_aws_tests.erl`
