# Secrets Backends - erlmcp Technical Guide

## Overview

erlmcp provides three secrets storage backends: HashiCorp Vault, AWS Secrets Manager, and local encrypted storage. This document covers backend architecture, implementation details, security considerations, and operational guidance.

**Module**: `erlmcp_secrets` (gen_server)
**Encryption**: AES-256-GCM for local storage
**Authentication**: Token, AppRole, IAM Role, Access Key
**Caching**: ETS with configurable TTL

## Architecture

### Backend Selection Strategy

```
┌──────────────────┐
│ erlmcp_secrets   │
│   (gen_server)   │
└────────┬─────────┘
         │
         ├─ Backend Config
         │  - vault
         │  - aws_secrets_manager
         │  - local_encrypted
         │
         ▼
┌─────────────────────────────────────────────┐
│            ETS Cache (TTL-based)             │
│   Key: binary() → {Value, ExpiresAt}        │
└─────────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────────┐
│          Backend Selection Logic             │
│                                              │
│  vault:           HTTP API (gun)            │
│  aws:             AWS API v4 signature      │
│  local_encrypted: AES-256-GCM file I/O      │
└─────────────────────────────────────────────┘
```

### State Record

```erlang
-record(state, {
    cache :: ets:tid(),              % Secret cache
    backend :: vault | aws_secrets_manager | local_encrypted,
    backend_config :: map(),         % Backend-specific config
    encryption_key :: binary(),      % 32-byte AES key
    ttl_seconds :: pos_integer(),    % Cache TTL (default: 300)
    storage_path :: file:filename()  % Local storage path
}).
```

## Backend 1: HashiCorp Vault

### Architecture

Vault integration uses:
- **HTTP API** via `gun` (HTTP/2 client)
- **KV v2 secrets engine** (versioned secrets)
- **Three auth methods**: Token, AppRole, Kubernetes
- **Automatic token renewal** (60s before expiry)
- **Circuit breaker** (future: fail fast on repeated errors)

### Authentication Methods

#### 1. Token Authentication (Development)

**Use Case**: Local development, testing

**Configuration**:
```erlang
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        url => <<"http://localhost:8200">>,
        token => <<"s.ABC123DEF456">>,
        auth_method => token,
        mount => <<"secret">>,
        namespace => undefined,
        timeout => 5000
    }}
]}.
```

**Security**:
- ⚠️ **Token stored in config** (not recommended for production)
- No automatic renewal
- Suitable for short-lived dev environments

#### 2. AppRole Authentication (Production)

**Use Case**: Production servers, CI/CD pipelines

**Configuration**:
```erlang
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        url => <<"https://vault.example.com:8200">>,
        auth_method => approle,
        role_id => <<"12345678-1234-1234-1234-123456789012">>,
        secret_id => <<"87654321-4321-4321-4321-210987654321">>,
        mount => <<"secret">>,
        namespace => <<"production">>,
        timeout => 5000
    }}
]}.
```

**Vault Setup**:
```bash
# Enable AppRole auth
vault auth enable approle

# Create policy
vault policy write erlmcp-policy - <<EOF
path "secret/data/*" {
  capabilities = ["create", "read", "update", "delete", "list"]
}
path "secret/metadata/*" {
  capabilities = ["list", "read"]
}
EOF

# Create AppRole
vault write auth/approle/role/erlmcp \
    token_policies="erlmcp-policy" \
    token_ttl=1h \
    token_max_ttl=4h

# Get Role ID
vault read auth/approle/role/erlmcp/role-id

# Generate Secret ID
vault write -f auth/approle/role/erlmcp/secret-id
```

**Authentication Flow**:
```
1. Client sends {role_id, secret_id} to /v1/auth/approle/login
2. Vault returns {client_token, lease_duration}
3. erlmcp_secrets stores token and calculates expiry
4. Token auto-renewed 60s before expiry
5. Subsequent requests use client_token
```

**Token Refresh**:
```erlang
-spec ensure_authenticated(vault_state()) -> {ok, vault_state()} | {error, term()}.
ensure_authenticated(#vault_state{token = Token, token_expiry = Expiry} = State) ->
    Now = erlang:timestamp(),
    case {Token, Expiry} of
        {undefined, _} -> authenticate(State);
        {_, undefined} -> {ok, State};  % Token auth (no expiry)
        {_, ExpiryTime} when Now >= ExpiryTime -> authenticate(State);
        _ -> {ok, State}
    end.
```

#### 3. Kubernetes Authentication (K8s Deployments)

**Use Case**: Kubernetes pods with service accounts

**Configuration**:
```erlang
{erlmcp_secrets, [
    {backend, vault},
    {backend_config, #{
        url => <<"http://vault.default.svc.cluster.local:8200">>,
        auth_method => kubernetes,
        k8s_role => <<"erlmcp-role">>,
        k8s_jwt_path => <<"/var/run/secrets/kubernetes.io/serviceaccount/token">>,
        mount => <<"secret">>,
        timeout => 5000
    }}
]}.
```

**Vault Setup**:
```bash
# Enable Kubernetes auth
vault auth enable kubernetes

# Configure Kubernetes auth
vault write auth/kubernetes/config \
    token_reviewer_jwt="$(cat /var/run/secrets/kubernetes.io/serviceaccount/token)" \
    kubernetes_host="https://kubernetes.default.svc:443" \
    kubernetes_ca_cert=@/var/run/secrets/kubernetes.io/serviceaccount/ca.crt

# Create role
vault write auth/kubernetes/role/erlmcp-role \
    bound_service_account_names=erlmcp \
    bound_service_account_namespaces=production \
    policies=erlmcp-policy \
    ttl=1h
```

**K8s Deployment Manifest**:
```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: erlmcp
  namespace: production
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-server
spec:
  template:
    spec:
      serviceAccountName: erlmcp
      containers:
      - name: erlmcp
        image: erlmcp:latest
        env:
        - name: VAULT_ADDR
          value: "http://vault.default.svc.cluster.local:8200"
```

### Vault KV v2 API

**Path Structure**:
```
/v1/{namespace}/{mount}/data/{secret_path}    # Read/write secret data
/v1/{namespace}/{mount}/metadata/{secret_path} # Read/list metadata
```

**GET Secret**:
```erlang
vault_get(Key, Config) ->
    Path = build_vault_path(<<"data">>, Key, VaultState),
    %% Path: "/v1/production/secret/data/api-keys/stripe"

    case vault_http_request(get, Path, <<>>, VaultState) of
        {ok, ResponseBody} ->
            %% Response: {"data": {"data": {"value": "sk_live_..."}}}
            parse_vault_secret_response(ResponseBody);
        {error, Reason} ->
            {error, Reason}
    end.
```

**PUT Secret**:
```erlang
vault_set(Key, Value, Config) ->
    Path = build_vault_path(<<"data">>, Key, VaultState),
    Body = jsx:encode(#{<<"data">> => #{<<"value">> => Value}}),

    case vault_http_request(post, Path, Body, VaultState) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
```

**DELETE Secret** (soft delete):
```erlang
vault_delete(Key, Config) ->
    Path = build_vault_path(<<"data">>, Key, VaultState),

    case vault_http_request(delete, Path, <<>>, VaultState) of
        {ok, _} -> ok;
        {error, Reason} -> {error, Reason}
    end.
```

**LIST Secrets**:
```erlang
vault_list(Config) ->
    Path = build_vault_path(<<"metadata">>, <<>>, VaultState) ++ "?list=true",

    case vault_http_request(get, Path, <<>>, VaultState) of
        {ok, ResponseBody} ->
            %% Response: {"data": {"keys": ["api-key", "db-password"]}}
            parse_vault_list_response(ResponseBody);
        {error, Reason} ->
            {error, Reason}
    end.
```

### HTTP Client Implementation

**Using `gun` for HTTP/2**:
```erlang
vault_http_request_raw(Method, VaultUrl, Path, Body, Headers, Timeout) ->
    #{host := Host, port := Port, scheme := Scheme} = uri_string:parse(VaultUrl),

    Transport = case Scheme of
        <<"https">> -> tls;
        <<"http">> -> tcp
    end,

    {ok, ConnPid} = gun:open(Host, Port, #{transport => Transport, protocols => [http]}),
    {ok, _Protocol} = gun:await_up(ConnPid, Timeout),

    StreamRef = case Method of
        get -> gun:get(ConnPid, Path, maps:to_list(Headers));
        post -> gun:post(ConnPid, Path, maps:to_list(Headers), Body);
        delete -> gun:delete(ConnPid, Path, maps:to_list(Headers))
    end,

    case gun:await(ConnPid, StreamRef, Timeout) of
        {response, nofin, 200, _} ->
            {ok, ResponseBody} = gun:await_body(ConnPid, StreamRef, Timeout),
            gun:close(ConnPid),
            {ok, ResponseBody};
        {response, fin, Status, Headers} ->
            gun:close(ConnPid),
            {error, {http_error, Status, Headers}}
    end.
```

### Error Handling

**Vault Errors**:
```erlang
parse_vault_secret_response(ResponseBody) ->
    case jsx:decode(ResponseBody, [return_maps]) of
        #{<<"data">> := #{<<"data">> := Data}} ->
            {ok, maps:get(<<"value">>, Data)};
        #{<<"errors">> := Errors} ->
            %% Vault returned errors
            {error, {vault_errors, Errors}};
        _ ->
            {error, invalid_response_format}
    end.
```

**Common Error Codes**:
- **403**: Permission denied (check policy)
- **404**: Secret not found
- **500**: Vault server error
- **503**: Vault sealed or uninitialized

## Backend 2: AWS Secrets Manager

### Architecture

AWS Secrets Manager integration uses:
- **AWS Signature Version 4** for request signing
- **IAM Role authentication** (preferred) or access keys
- **Automatic credential refresh** from EC2/ECS metadata service
- **STS AssumeRole** support for cross-account access

### Authentication Methods

#### 1. IAM Role (EC2/ECS)

**Use Case**: Production deployments on AWS

**Configuration**:
```erlang
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => <<"us-east-1">>,
        auth_method => iam_role,
        enabled => true,
        timeout => 5000
    }}
]}.
```

**IAM Policy** (attach to EC2/ECS role):
```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "secretsmanager:GetSecretValue",
        "secretsmanager:CreateSecret",
        "secretsmanager:UpdateSecret",
        "secretsmanager:DeleteSecret",
        "secretsmanager:ListSecrets"
      ],
      "Resource": "arn:aws:secretsmanager:*:*:secret:erlmcp/*"
    }
  ]
}
```

**Credential Retrieval**:
```erlang
get_iam_role_credentials(Config) ->
    %% EC2 metadata service
    MetadataUrl = <<"http://169.254.169.254/latest/meta-data/iam/security-credentials/">>,

    %% Get role name
    {ok, {{_, 200, _}, _, RoleName}} = httpc:request(get, {MetadataUrl, []}, [], []),

    %% Get credentials
    RoleUrl = <<MetadataUrl/binary, RoleName/binary>>,
    {ok, {{_, 200, _}, _, CredsJson}} = httpc:request(get, {RoleUrl, []}, [], []),

    #{
        <<"AccessKeyId">> := AccessKey,
        <<"SecretAccessKey">> := SecretKey,
        <<"Token">> := Token,
        <<"Expiration">> := Expiry
    } = jsx:decode(CredsJson, [return_maps]),

    {ok, #aws_credentials{
        access_key_id = AccessKey,
        secret_access_key = SecretKey,
        token = Token,
        expiration = parse_iso8601(Expiry)
    }}.
```

#### 2. Access Key / Secret Key

**Use Case**: Local development, non-AWS environments

**Configuration**:
```erlang
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => <<"us-east-1">>,
        auth_method => access_key,
        access_key => <<"AKIAIOSFODNN7EXAMPLE">>,
        secret_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
        enabled => true
    }}
]}.
```

**Security**:
- ⚠️ **Credentials in config** (not recommended)
- Use environment variables or AWS SSM Parameter Store instead

#### 3. AssumeRole (Cross-Account)

**Use Case**: Access secrets in different AWS account

**Configuration**:
```erlang
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => <<"us-east-1">>,
        auth_method => iam_role,
        role_arn => <<"arn:aws:iam::123456789012:role/SecretAccessRole">>,
        role_duration => 3600,
        enabled => true
    }}
]}.
```

**STS AssumeRole**:
```erlang
assume_role(BaseCreds, RoleArn, Config) ->
    Region = maps:get(region, Config, <<"us-east-1">>),
    Duration = maps:get(role_duration, Config, 3600),

    Params = #{
        <<"RoleArn">> => RoleArn,
        <<"RoleSessionName">> => <<"erlmcp-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
        <<"DurationSeconds">> => Duration
    },

    case make_aws_request(Region, <<"sts">>, BaseCreds, post, <<"/">>,
                         #{<<"X-Amz-Target">> => <<"sts.AssumeRole">>},
                         jsx:encode(Params), Config) of
        {ok, ResponseBody} ->
            #{<<"Credentials">> := #{
                <<"AccessKeyId">> := NewAccessKey,
                <<"SecretAccessKey">> := NewSecretKey,
                <<"SessionToken">> := NewToken
            }} = jsx:decode(ResponseBody, [return_maps]),
            {ok, #aws_credentials{
                access_key_id = NewAccessKey,
                secret_access_key = NewSecretKey,
                token = NewToken
            }};
        {error, Reason} ->
            {error, {assume_role_failed, Reason}}
    end.
```

### AWS Signature Version 4

**Algorithm**:
```
1. Canonical Request = HTTP_METHOD + "\n" +
                       CANONICAL_URI + "\n" +
                       CANONICAL_QUERY_STRING + "\n" +
                       CANONICAL_HEADERS + "\n" +
                       SIGNED_HEADERS + "\n" +
                       HEX(SHA256(PAYLOAD))

2. String to Sign = ALGORITHM + "\n" +
                    TIMESTAMP + "\n" +
                    CREDENTIAL_SCOPE + "\n" +
                    HEX(SHA256(CANONICAL_REQUEST))

3. Signing Key = HMAC-SHA256(HMAC-SHA256(HMAC-SHA256(HMAC-SHA256(
                    "AWS4" + SECRET_KEY, DATE), REGION), SERVICE), "aws4_request")

4. Signature = HEX(HMAC-SHA256(SIGNING_KEY, STRING_TO_SIGN))

5. Authorization Header = ALGORITHM + " Credential=" + ACCESS_KEY + "/" + CREDENTIAL_SCOPE +
                         ", SignedHeaders=" + SIGNED_HEADERS +
                         ", Signature=" + SIGNATURE
```

**Implementation**:
```erlang
calculate_sigv4(Method, Host, Path, ExtraHeaders, Body, Region, Service, Creds, AmzDate, DateStamp) ->
    %% Canonical headers (lowercase, sorted)
    CanonicalHeaders = [
        {<<"content-type">>, <<"application/x-amz-json-1.1">>},
        {<<"host">>, Host},
        {<<"x-amz-date">>, AmzDate}
    ] ++ maps:to_list(ExtraHeaders),

    SortedHeaders = lists:sort(fun({A, _}, {B, _}) ->
        string:lowercase(binary_to_list(A)) =< string:lowercase(binary_to_list(B))
    end, CanonicalHeaders),

    SignedHeaders = list_to_binary(string:join(
        [string:lowercase(binary_to_list(K)) || {K, _} <- SortedHeaders], ";"
    )),

    %% Canonical request
    PayloadHash = crypto:hash(sha256, Body),
    CanonicalRequest = iolist_to_binary([
        string:uppercase(atom_to_list(Method)), "\n",
        Path, "\n",
        <<>>, "\n",  % Query string
        << <<(list_to_binary(string:lowercase(binary_to_list(K))))/binary, ":", V/binary, "\n">>
           || {K, V} <- SortedHeaders >>,
        "\n",
        SignedHeaders, "\n",
        hex_encode(PayloadHash)
    ]),

    %% String to sign
    CredentialScope = <<DateStamp/binary, "/", Region/binary, "/", Service/binary, "/aws4_request">>,
    StringToSign = <<"AWS4-HMAC-SHA256\n", AmzDate/binary, "\n", CredentialScope/binary, "\n",
                     (hex_encode(crypto:hash(sha256, CanonicalRequest)))/binary>>,

    %% Signing key
    KDate = hmac_sha256(<<"AWS4", (Creds#aws_credentials.secret_access_key)/binary>>, DateStamp),
    KRegion = hmac_sha256(KDate, Region),
    KService = hmac_sha256(KRegion, Service),
    KSigning = hmac_sha256(KService, <<"aws4_request">>),

    %% Signature
    Signature = hmac_sha256(KSigning, StringToSign),

    %% Authorization header
    Authorization = <<"AWS4-HMAC-SHA256 Credential=",
                      (Creds#aws_credentials.access_key_id)/binary, "/", CredentialScope/binary,
                      ", SignedHeaders=", SignedHeaders/binary,
                      ", Signature=", (hex_encode(Signature))/binary>>,

    #{
        <<"Authorization">> => Authorization,
        <<"X-Amz-Date">> => AmzDate,
        <<"Content-Type">> => <<"application/x-amz-json-1.1">>
    }.
```

### AWS API Operations

**GetSecretValue**:
```erlang
aws_secrets_get(SecretId, Config) ->
    Region = maps:get(region, Config, <<"us-east-1">>),
    {ok, Creds} = get_aws_credentials(maps:get(auth_method, Config), Config),

    RequestBody = jsx:encode(#{
        <<"SecretId">> => SecretId,
        <<"VersionStage">> => <<"AWSCURRENT">>
    }),

    case make_aws_request(Region, <<"secretsmanager">>, Creds, post, <<"/">>,
                         #{<<"X-Amz-Target">> => <<"secretsmanager.GetSecretValue">>},
                         RequestBody, Config) of
        {ok, ResponseBody} ->
            #{<<"SecretString">> := SecretValue} = jsx:decode(ResponseBody, [return_maps]),
            {ok, SecretValue};
        {error, Reason} ->
            {error, Reason}
    end.
```

**CreateSecret / UpdateSecret**:
```erlang
aws_secrets_set(SecretId, SecretValue, Config) ->
    Region = maps:get(region, Config, <<"us-east-1">>),
    {ok, Creds} = get_aws_credentials(maps:get(auth_method, Config), Config),

    %% Try creating first
    CreateParams = #{<<"Name">> => SecretId, <<"SecretString">> => SecretValue},
    case make_aws_request(Region, <<"secretsmanager">>, Creds, post, <<"/">>,
                         #{<<"X-Amz-Target">> => <<"secretsmanager.CreateSecret">>},
                         jsx:encode(CreateParams), Config) of
        {ok, _} -> ok;
        {error, {http_error, 400, _}} ->
            %% Secret exists, update instead
            UpdateParams = #{<<"SecretId">> => SecretId, <<"SecretString">> => SecretValue},
            case make_aws_request(Region, <<"secretsmanager">>, Creds, post, <<"/">>,
                                 #{<<"X-Amz-Target">> => <<"secretsmanager.UpdateSecret">>},
                                 jsx:encode(UpdateParams), Config) of
                {ok, _} -> ok;
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

## Backend 3: Local Encrypted Storage

### Architecture

Local storage uses:
- **AES-256-GCM** encryption (authenticated encryption)
- **32-byte encryption key** (generated or loaded from file)
- **12-byte random IV** per encryption (nonce)
- **16-byte authentication tag** (prevents tampering)

### Encryption Implementation

**Key Generation**:
```erlang
load_or_generate_encryption_key(Config) ->
    KeyPath = maps:get(encryption_key_path, Config, "priv/secrets/master.key"),

    case file:read_file(KeyPath) of
        {ok, Key} when byte_size(Key) =:= 32 ->
            Key;
        _ ->
            %% Generate new 256-bit key
            NewKey = crypto:strong_rand_bytes(32),
            ok = filelib:ensure_dir(filename:dirname(KeyPath) ++ "/"),
            ok = file:write_file(KeyPath, NewKey),
            os:cmd("chmod 600 " ++ KeyPath),  % Unix only
            logger:warning("Generated new encryption key: ~s", [KeyPath]),
            NewKey
    end.
```

**Encryption (AES-256-GCM)**:
```erlang
encrypt_aes_gcm(PlainText, Key) ->
    IV = crypto:strong_rand_bytes(12),  % 96-bit nonce
    {CipherText, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, PlainText, <<>>, true),
    <<IV:12/binary, Tag:16/binary, CipherText/binary>>.
```

**Decryption**:
```erlang
decrypt_aes_gcm(<<IV:12/binary, Tag:16/binary, CipherText/binary>>, Key) ->
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, CipherText, <<>>, Tag, false).
```

**Storage Format**:
```
File: priv/secrets/secrets.enc

[12 bytes IV][16 bytes Tag][Variable-length Ciphertext]
                                    │
                                    └─> term_to_binary(#{
                                          <<"api_key">> => <<"sk_live_...">>,
                                          <<"db_password">> => <<"postgres123">>
                                        })
```

### Operations

**Get Secret**:
```erlang
local_get(Key, State) ->
    case load_encrypted_storage(State) of
        {ok, Secrets} ->
            case maps:get(Key, Secrets, undefined) of
                undefined -> {error, not_found};
                Value -> {ok, Value}
            end;
        {error, enoent} ->
            {error, not_found};
        Error ->
            Error
    end.
```

**Set Secret**:
```erlang
local_set(Key, Value, State) ->
    Secrets = case load_encrypted_storage(State) of
        {ok, S} -> S;
        {error, enoent} -> #{}
    end,

    NewSecrets = maps:put(Key, Value, Secrets),
    save_encrypted_storage(NewSecrets, State).
```

**Security Considerations**:
- **Key protection**: Master key stored with `chmod 600`
- **Memory safety**: Secrets in Erlang heap (GC may leave traces)
- **Tampering detection**: GCM tag validates authenticity
- **No key derivation**: Use strong random keys, not passwords

## Caching Strategy

### ETS Cache

**Configuration**:
```erlang
Cache = ets:new(secrets_cache, [
    set,           % Key-value store
    protected,     % Only owner can write
    {read_concurrency, true},   % Optimize concurrent reads
    {write_concurrency, true}   % Optimize concurrent writes
]),
```

**Cache Entry Format**:
```erlang
{Key :: binary(), {Value :: binary(), ExpiresAt :: integer()}}
```

**TTL Management**:
```erlang
%% On get
do_get_secret(Key, State) ->
    Now = erlang:system_time(second),
    case ets:lookup(State#state.cache, Key) of
        [{_, {Value, ExpiresAt}}] when ExpiresAt > Now ->
            {ok, Value};  % Cache hit
        _ ->
            %% Cache miss or expired
            {ok, Value} = fetch_from_backend(Key, State),
            ExpiresAt = Now + State#state.ttl_seconds,
            ets:insert(State#state.cache, {Key, {Value, ExpiresAt}}),
            {ok, Value}
    end.

%% Periodic cleanup (every 60s)
handle_info(cleanup_cache, State) ->
    Now = erlang:system_time(second),
    ets:foldl(fun({Key, {_Value, ExpiresAt}}, Acc) ->
        case ExpiresAt < Now of
            true -> ets:delete(State#state.cache, Key);
            false -> ok
        end,
        Acc
    end, ok, State#state.cache),
    erlang:send_after(60000, self(), cleanup_cache),
    {noreply, State}.
```

## Secret Rotation

### Automatic Rotation

```erlang
-spec rotate_secret(secret_key()) -> {ok, secret_value()} | {error, term()}.

rotate_secret(Key) ->
    %% Generate new 32-byte random secret
    NewValue = base64:encode(crypto:strong_rand_bytes(32)),

    %% Store in backend
    case erlmcp_secrets:set_secret(Key, NewValue) of
        ok ->
            logger:info("Secret rotated: ~p", [Key]),
            {ok, NewValue};
        Error ->
            Error
    end.
```

### Scheduled Rotation (Future)

```erlang
%% Cron-style rotation
{erlmcp_secrets, [
    {rotation_schedule, [
        {<<"api_keys/stripe">>, {daily, {3, 0, 0}}},     % 3 AM daily
        {<<"db/password">>, {weekly, sunday, {2, 0, 0}}}, % 2 AM Sundays
        {<<"oauth/tokens">>, {hourly, 0}}                % Every hour
    ]}
]}.
```

## Security Best Practices

### 1. Vault

- ✅ Use AppRole or Kubernetes auth in production
- ✅ Enable TLS (HTTPS)
- ✅ Use namespaces for multi-tenancy
- ✅ Rotate AppRole secret_id regularly
- ✅ Monitor audit logs
- ❌ Never commit Vault tokens to git
- ❌ Don't use root tokens for services

### 2. AWS Secrets Manager

- ✅ Use IAM roles (EC2/ECS/Lambda)
- ✅ Implement least-privilege policies
- ✅ Enable CloudTrail logging
- ✅ Use resource-based policies
- ✅ Rotate secrets automatically
- ❌ Don't hardcode access keys
- ❌ Don't share credentials across environments

### 3. Local Encrypted

- ✅ Store master key outside codebase
- ✅ Use environment variables for key path
- ✅ Restrict file permissions (`chmod 600`)
- ✅ Backup encrypted secrets file
- ❌ Don't commit master key to git
- ❌ Not suitable for production (use Vault/AWS)

## Operational Guidance

### Migrating Between Backends

**Vault → AWS**:
```erlang
migrate_vault_to_aws() ->
    %% List all Vault secrets
    {ok, VaultKeys} = erlmcp_secrets:list_secrets(),

    %% Reconfigure to AWS
    erlmcp_secrets:configure_aws(#{
        region => <<"us-east-1">>,
        auth_method => iam_role
    }),

    %% Migrate each secret
    lists:foreach(fun(Key) ->
        {ok, Value} = fetch_from_vault(Key),
        ok = erlmcp_secrets:set_secret(Key, Value),
        logger:info("Migrated: ~s", [Key])
    end, VaultKeys).
```

### Disaster Recovery

**Backup (local encrypted)**:
```bash
cp priv/secrets/secrets.enc /backup/secrets-$(date +%Y%m%d).enc
cp priv/secrets/master.key /secure-location/master.key
```

**Restore**:
```bash
cp /backup/secrets-20260131.enc priv/secrets/secrets.enc
cp /secure-location/master.key priv/secrets/master.key
chmod 600 priv/secrets/master.key
```

### Monitoring

**Metrics**:
```erlang
erlmcp_metrics:increment(secrets_fetch, #{backend => vault, cache_hit => false}),
erlmcp_metrics:observe(secrets_latency_ms, Latency, #{backend => aws}),
erlmcp_metrics:increment(secrets_rotation, #{key => Key}).
```

**Health Checks**:
```erlang
secrets_health() ->
    TestKey = <<"_health_check">>,
    TestValue = <<"ok">>,

    case erlmcp_secrets:set_secret(TestKey, TestValue) of
        ok ->
            case erlmcp_secrets:get_secret(TestKey) of
                {ok, TestValue} ->
                    erlmcp_secrets:delete_secret(TestKey),
                    {healthy, #{backend => vault}};
                _ ->
                    {degraded, #{backend => vault, error => read_failed}}
            end;
        _ ->
            {unhealthy, #{backend => vault, error => write_failed}}
    end.
```

## Performance

**Throughput (cached)**:
- Get: ~5M ops/sec (ETS read)
- Set: ~100K ops/sec (backend + cache invalidation)

**Latency (uncached)**:
- Vault: ~10-50ms (HTTP round-trip)
- AWS: ~50-200ms (SigV4 signing + HTTP)
- Local: <1ms (file I/O)

**Optimization**:
- Increase cache TTL for read-heavy workloads
- Use read replicas (Vault Enterprise)
- Batch secret fetches where possible

## Future Enhancements

1. **Vault dynamic secrets** - Database credentials, AWS creds
2. **AWS Secrets Manager rotation lambdas** - Automatic secret rotation
3. **Azure Key Vault** - Fourth backend
4. **Secret versioning** - Access previous secret values
5. **Audit trail** - Track secret access patterns
