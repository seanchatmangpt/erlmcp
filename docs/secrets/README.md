# Secrets Management Architecture - erlmcp

## Overview

erlmcp provides enterprise-grade secrets management with support for HashiCorp Vault, AWS Secrets Manager, and encrypted local storage. Secrets are cached in-memory with TTL for performance, encrypted at rest using AES-256-GCM, and all access is logged for compliance.

## Table of Contents

- [Architecture](#architecture)
- [Backend Comparison](#backend-comparison)
- [Encryption Details](#encryption-details)
- [Secret Lifecycle](#secret-lifecycle)
- [Caching Strategy](#caching-strategy)
- [Integration Patterns](#integration-patterns)
- [Security Best Practices](#security-best-practices)

## Architecture

### High-Level Components

```mermaid
graph TB
    subgraph "Application Layer"
        App1[erlmcp_server]
        App2[erlmcp_client]
        App3[Custom Apps]
    end

    subgraph "Secrets API Layer"
        API[erlmcp_secrets API]
        Cache[ETS Cache<br/>TTL: 5min]
        Monitor[Access Monitor]
    end

    subgraph "Backend Adapters"
        Vault[Vault Adapter]
        AWS[AWS Adapter]
        Local[Local Encrypted Adapter]
    end

    subgraph "Storage Backends"
        VaultStore[HashiCorp Vault<br/>KV v2 Engine]
        AWSStore[AWS Secrets Manager<br/>KMS Encryption]
        LocalStore[Local File<br/>AES-256-GCM]
    end

    subgraph "Observability"
        Audit[Audit Log]
        Metrics[Metrics]
        Alerts[Security Alerts]
    end

    App1 -->|get_secret/1| API
    App2 -->|get_secret/1| API
    App3 -->|get_secret/1| API

    API -->|Check Cache| Cache

    API -->|Cache Hit| Cache
    API -->|Cache Miss| Vault
    API -->|Cache Miss| AWS
    API -->|Cache Miss| Local

    Vault -->|HTTP/REST| VaultStore
    AWS -->|SDK + HTTPS| AWSStore
    Local -->|File I/O| LocalStore

    API -->|Log Access| Audit
    API -->|Record Metrics| Metrics
    Audit -->|Anomaly Detection| Alerts

    Cache -.->|TTL Expired| Invalidate[Invalidate Entry]

    style API fill:#4ecdc4,stroke:#0ca678,stroke-width:3px
    style Cache fill:#ffe3e3,stroke:#c92a2a,stroke-width:2px
    style VaultStore fill:#7950f2,stroke:#5f3dc4,stroke-width:2px
    style AWSStore fill:#ff922b,stroke:#e67700,stroke-width:2px
    style LocalStore fill:#51cf66,stroke:#2f9e44,stroke-width:2px
```

### Request Flow

```mermaid
sequenceDiagram
    participant App as Application
    participant API as erlmcp_secrets
    participant Cache as ETS Cache
    participant Backend as Backend Adapter
    participant Store as Storage
    participant Audit as Audit Log

    App->>API: get_secret(<<"db/password">>)

    API->>Cache: Lookup Key

    alt Cache Hit (Valid TTL)
        Cache-->>API: Secret Value
        API->>Audit: Log Cache Hit
        API-->>App: {ok, Secret}
    else Cache Miss
        API->>Backend: Fetch Secret
        Backend->>Store: Read Secret
        Store-->>Backend: Encrypted Secret
        Backend->>Backend: Decrypt
        Backend-->>API: Plaintext Secret
        API->>Cache: Store with TTL
        API->>Audit: Log Backend Access
        API-->>App: {ok, Secret}
    end

    App->>API: set_secret(<<"db/password">>, Value)
    API->>Backend: Store Secret
    Backend->>Backend: Encrypt
    Backend->>Store: Write Encrypted
    Store-->>Backend: OK
    API->>Cache: Invalidate Key
    API->>Audit: Log Secret Write
    API-->>App: ok

    App->>API: rotate_secret(<<"db/password">>)
    API->>API: Generate Random Value (32 bytes)
    API->>Backend: Store New Secret
    Backend->>Store: Update Secret
    API->>Cache: Invalidate Key
    API->>Audit: Log Secret Rotation
    API-->>App: {ok, NewSecret}
```

## Backend Comparison

### Feature Matrix

```mermaid
graph LR
    subgraph "Vault"
        V1[Enterprise Features]
        V2[Dynamic Secrets]
        V3[Encryption as Service]
        V4[Audit Integration]
        V5[High Availability]
    end

    subgraph "AWS Secrets Manager"
        A1[Native AWS Integration]
        A2[Automatic Rotation]
        A3[KMS Encryption]
        A4[ IAM Policies]
        A5[Global Availability]
    end

    subgraph "Local Encrypted"
        L1[Zero Dependencies]
        L2[AES-256-GCM]
        L3[Offline Operation]
        L4[Fast Performance]
        L5[Simplicity]
    end

    style V1 fill:#e3fafc,stroke:#0ca678
    style V2 fill:#e3fafc,stroke:#0ca678
    style V3 fill:#e3fafc,stroke:#0ca678
    style A1 fill:#fff3bf,stroke:#fab005
    style A2 fill:#fff3bf,stroke:#fab005
    style A3 fill:#fff3bf,stroke:#fab005
    style L1 fill:#ffe3e3,stroke:#c92a2a
    style L2 fill:#ffe3e3,stroke:#c92a2a
    style L3 fill:#ffe3e3,stroke:#c92a2a
```

### Decision Tree

```mermaid
graph TB
    Start[Choose Secrets Backend] --> Q1{Running on<br/>AWS?}

    Q1 -->|Yes| Q2{Need AWS<br/>Integration?}
    Q1 -->|No| Q3{Have Vault<br/>Infrastructure?}

    Q2 -->|Yes| AWS[AWS Secrets Manager]
    Q2 -->|No| Q3

    Q3 -->|Yes| Q4{Need<br/>Dynamic Secrets?}
    Q3 -->|No| Q5{Need Zero<br/>Dependencies?}

    Q4 -->|Yes| Vault[HashiCorp Vault]
    Q4 -->|No| Q5

    Q5 -->|Yes| Local[Local Encrypted]
    Q5 -->|No| Vault

    style AWS fill:#ff922b,stroke:#e67700,stroke-width:3px
    style Vault fill:#7950f2,stroke:#5f3dc4,stroke-width:3px
    style Local fill:#51cf66,stroke:#2f9e44,stroke-width:3px
```

## Encryption Details

### Local Storage Encryption

```mermaid
graph TB
    subgraph "Encryption Flow"
        Secret[Plaintext Secret] --> Generate[Generate Random IV]
        Generate --> Derive[Derive Key from Master Key]
        Derive --> Encrypt[AES-256-GCM Encrypt]
        Encrypt --> Tag[Generate Auth Tag]
        Tag --> Store[Store: IV || Ciphertext || Tag]
    end

    subgraph "Decryption Flow"
        Stored[IV || Ciphertext || Tag] --> Parse[Parse Components]
        Parse --> Derive2[Derive Key from Master Key]
        Derive2 --> Decrypt[AES-256-GCM Decrypt]
        Decrypt --> Verify[Verify Auth Tag]
        Verify -->|Valid| Plaintext[Return Plaintext]
        Verify -->|Invalid| Error[Return Error: Tampering Detected]
    end

    subgraph "Key Management"
        EnvVar[Environment Variable<br/>ERLMCP_SECRET_KEY] --> Decode[Base64 Decode]
        File[Master Key File<br/>priv/secrets/master.key] --> Read[Read File]
        Decode --> MasterKey[Master Key]
        Read --> MasterKey
    end

    Derive --> MasterKey
    Derive2 --> MasterKey

    style Encrypt fill:#ffe3e3,stroke:#c92a2a,stroke-width:2px
    style Decrypt fill:#d3f9d8,stroke:#37b24d,stroke-width:2px
    style MasterKey fill:#ff6b6b,stroke:#c92a2a,stroke-width:3px
    style Error fill:#ff6b6b,stroke:#c92a2a,stroke-width:3px
```

### Security Properties

- **Algorithm**: AES-256-GCM (Galois/Counter Mode)
- **Key Size**: 256 bits
- **IV/Nonce**: 96 bits (randomly generated per encryption)
- **Auth Tag**: 128 bits (detects tampering)
- **Key Derivation**: PBKDF2 with SHA-256 (100,000 iterations)
- **Master Key Storage**: Environment variable or file (600 permissions)

## Secret Lifecycle

### State Machine

```mermaid
stateDiagram-v2
    [*] --> Created: set_secret/2
    Created --> Cached: Store in ETS cache
    Cached --> Active: Valid TTL
    Cached --> Expired: TTL expires
    Active --> Active: get_secret/1 (cache hit)
    Active --> Expired: TTL expires
    Expired --> Active: get_secret/1 (cache miss, refresh)
    Active --> Updated: set_secret/2 (new value)
    Updated --> Cached: Update cache
    Active --> Rotated: rotate_secret/1
    Rotated --> Cached: New value cached
    Active --> Deleted: delete_secret/1
    Deleted --> [*]: Remove from backend and cache

    note right of Cached
        ETS table: secrets_cache
        TTL: 300 seconds (default)
        Scope: Per-node memory
    end note

    note right of Rotated
        Generates 32-byte random value
        Invalidates old cache entry
        Logs rotation event
    end note
```

### Caching Strategy

```mermaid
graph TB
    subgraph "Cache Entry Structure"
        Entry[#secrets_cache record]
        Key[key: binary()]
        Value[value: binary()]
        InsertedAt[inserted_at: timestamp()]
        TTL[ttl: integer()]
        Hits[hits: integer()]
    end

    subgraph "Cache Operations"
        Read[Read Operation<br/>O(1) ETS lookup]
        Write[Write Operation<br/>O(1) ETS insert]
        Invalidate[Invalidate<br/>O(1) ETS delete]
        Cleanup[Cleanup Expired<br/>O(N) periodic scan]
    end

    subgraph "Cache Statistics"
        CacheHits[Cache Hits]
        CacheMisses[Cache Misses]
        HitRate[Hit Rate = Hits / (Hits + Misses)]
        AvgTTL[Average TTL]
    end

    Read --> Entry
    Write --> Entry
    Invalidate --> Entry
    Cleanup --> Entry

    Read --> CacheHits
    Read -.-> CacheMisses

    style Entry fill:#d0ebff,stroke:#1c7ed6,stroke-width:2px
    style Read fill:#d3f9d8,stroke:#37b24d,stroke-width:2px
    style Invalidate fill:#ffe3e3,stroke:#c92a2a,stroke-width:2px
```

## Integration Patterns

### Pattern 1: Application Startup

```mermaid
sequenceDiagram
    participant App as Application
    participant Secrets as erlmcp_secrets
    participant Vault as Vault Backend
    participant DB as Database

    App->>Secrets: start_link/1
    Secrets->>Vault: Connect and Authenticate
    Vault-->>Secrets: Connection Established
    Secrets-->>App: {ok, Pid}

    App->>Secrets: get_secret(<<"db/password">>)
    Secrets->>Vault: Fetch Secret
    Vault-->>Secrets: Password
    Secrets->>Secrets: Cache Secret (TTL: 300s)
    Secrets-->>App: {ok, Password}

    App->>DB: Connect with Password
    DB-->>App: Connection Established

    Note over App,DB: Application runs with cached secret

    loop Every 5 minutes
        Secrets->>Secrets: Check cache TTL
        alt TTL Expired
            Secrets->>Secrets: Invalidate cache entry
        end
    end
```

### Pattern 2: Secret Rotation

```mermaid
sequenceDiagram
    participant Cron as Cron/Scheduler
    participant Secrets as erlmcp_secrets
    participant Vault as Vault Backend
    participant App as Application
    participant Dep as Dependent Service

    Cron->>Secrets: rotate_secret(<<"api/key">>)
    Secrets->>Secrets: Generate Random 32-byte value
    Secrets->>Vault: Update Secret
    Vault-->>Secrets: Updated
    Secrets->>Secrets: Invalidate Cache

    Secrets->>App: Broadcast secret_updated event
    App->>Dep: Reconnect with New Secret
    Dep-->>App: Connection Re-established

    Secrets-->>Cron: {ok, NewSecret}

    Note over Secrets,Dep: Zero-downtime rotation
```

### Pattern 3: Graceful Degradation

```mermaid
stateDiagram-v2
    [*] --> Healthy: Vault backend available
    Healthy --> Degraded: Vault connection fails
    Degraded --> Healthy: Vault recovers

    state Healthy {
        [*] --> VaultPrimary
        VaultPrimary: Read/write from Vault
        VaultPrimary: Cache all secrets
    }

    state Degraded {
        [*] --> FallbackMode
        FallbackMode: Read from cache (stale OK)
        FallbackMode: Write to local encrypted
        FallbackMode: Queue writes for replay
    }

    note right of Degraded
        Cache-aside pattern
        Local encrypted fallback
        Write-behind buffer
        Automatic retry
    end note
```

## Security Best Practices

### 1. Access Control

```mermaid
graph TB
    subgraph "Principle of Least Privilege"
        App1[App 1: Database Client] -->|Read only| DBSecret[database/password]
        App2[App 2: API Server] -->|Read only| APIKey[api/key]
        Admin[Admin Console] -->|Read/Write| AllSecrets[All Secrets]
    end

    subgraph "Vault Policy (HCL)"
        Policy[path "secret/data/erlmcp/db/*" {
  capabilities = ["read"]
}

path "secret/data/erlmcp/api/*" {
  capabilities = ["read", "update"]
}

path "secret/data/erlmcp/admin/*" {
  capabilities = ["create", "read", "update", "delete"]
}]
    end

    App1 --> Policy
    App2 --> Policy
    Admin --> Policy

    style Admin fill:#ffe3e3,stroke:#c92a2a,stroke-width:2px
```

### 2. Audit Trail

```mermaid
sequenceDiagram
    participant App as Application
    participant Secrets as erlmcp_secrets
    participant Audit as Audit Log
    participant Alert as Alert System

    App->>Secrets: get_secret(<<"sensitive/data">>)

    Secrets->>Audit: Log access
    Note over Audit: {
  "timestamp": "2025-01-31T12:00:00Z",
  "event": "secret_access",
  "key": "sensitive/data",
  "user": "app1@node1",
  "result": "success",
  "cache_hit": false
}

    Audit->>Alert: Check for anomalies
    alt Unusual access pattern
        Alert-->>Secrets: Alert: Potential breach
        Secrets-->>App: {error, suspicious_activity}
    else Normal access
        Secrets-->>App: {ok, Secret}
    end
```

### 3. Secret Rotation Schedule

```mermaid
gantt
    title Secret Rotation Timeline (90-day cycle)
    dateFormat  YYYY-MM-DD
    section Database Password
    Rotate            :done,    rot1, 2025-01-01, 1d
    Valid period      :active,  val1, 2025-01-02, 89d
    Next rotation     :         rot2, 2025-04-01, 1d

    section API Keys
    Rotate            :done,    rot3, 2025-01-15, 1d
    Valid period      :active,  val2, 2025-01-16, 89d
    Next rotation     :         rot4, 2025-04-15, 1d

    section TLS Certificates
    Rotate            :done,    rot5, 2025-01-01, 1d
    Valid period      :active,  val3, 2025-01-02, 89d
    Renew warning     :         warn, 2025-03-23, 7d
    Next rotation     :         rot6, 2025-04-01, 1d
```

## Performance Considerations

### Cache Hit Rate Targets

```mermaid
graph LR
    subgraph "Performance Metrics"
        HitRate[Cache Hit Rate<br/>Target: >95%]
        Latency[p50 Latency<br/><1ms (cache hit)<br/>50-100ms (cache miss)]
        Throughput[Throughput<br/>10K ops/sec (cached)]
    end

    subgraph "Optimization Strategies"
        S1[Set appropriate TTL<br/>Longer TTL = Higher hit rate]
        S2[Prefetch critical secrets<br/>Load at startup]
        S3[Batch operations<br/>Fetch multiple secrets]
        S4[Local fallback<br/>Reduce backend calls]
    end

    HitRate --> S1
    HitRate --> S2
    Latency --> S3
    Latency --> S4
    Throughput --> S4

    style HitRate fill:#d3f9d8,stroke:#37b24d,stroke-width:2px
    style Latency fill:#d3f9d8,stroke:#37b24d,stroke-width:2px
    style Throughput fill:#d3f9d8,stroke:#37b24d,stroke-width:2px
```

## Configuration Examples

### Development (Local Encrypted)

```erlang
{erlmcp_secrets, [
    {backend, local_encrypted},
    {encryption_key, {env_var, "ERLMCP_SECRET_KEY"}},
    {storage_path, "priv/secrets/dev.enc"},
    {ttl_seconds, 60},  % Short TTL for dev
    {audit_enabled, true}
]}.
```

### Production (Vault with AppRole)

```erlang
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
    {ttl_seconds, 600},  % 10 minutes for prod
    {audit_enabled, true},
    {fallback_to_local, true},
    {storage_path, "priv/secrets/fallback.enc"}
]}.
```

### Production (AWS with IAM Role)

```erlang
{erlmcp_secrets, [
    {backend, aws_secrets_manager},
    {backend_config, #{
        region => "us-east-1",
        use_iam_role => true,
        prefix => "erlmcp/prod/",
        rotation_enabled => true
    }},
    {ttl_seconds, 600},
    {audit_enabled, true}
]}.
```

## Monitoring & Observability

### Key Metrics

```mermaid
graph TB
    subgraph "Performance Metrics"
        M1[Cache Hit Rate]
        M2[Backend Latency]
        M3[Cache Size]
        M4[TTL Distribution]
    end

    subgraph "Security Metrics"
        S1[Failed Access Attempts]
        S2[Secret Rotation Events]
        S3[Audit Log Volume]
        S4[Unauthorized Access Attempts]
    end

    subgraph "Health Metrics"
        H1[Backend Connection Status]
        H2[Cache Health]
        H3[Disk Usage (local backend)]
        H4[Master Key Status]
    end

    M1 --> Dashboard[Observability Dashboard]
    M2 --> Dashboard
    M3 --> Dashboard
    M4 --> Dashboard
    S1 --> Alerts[Alert System]
    S2 --> Alerts
    S3 --> Alerts
    S4 --> Alerts
    H1 --> HealthCheck[Health Check Endpoint]
    H2 --> HealthCheck
    H3 --> HealthCheck
    H4 --> HealthCheck

    style Dashboard fill:#d0ebff,stroke:#1c7ed6,stroke-width:2px
    style Alerts fill:#ffe3e3,stroke:#c92a2a,stroke-width:2px
    style HealthCheck fill:#d3f9d8,stroke:#37b24d,stroke-width:2px
```

## Troubleshooting

### Common Issues and Solutions

| Symptom | Root Cause | Solution |
|---------|------------|----------|
| `{error, connection_refused}` | Vault backend unreachable | Check network, verify Vault URL |
| `{error, unauthorized}` | Invalid credentials | Verify Vault token / AWS credentials |
| Stale secret returned | Cache not invalidated | Manual cache invalidation or reduce TTL |
| `{error, encryption_failed}` | Master key invalid | Verify ERLMCP_SECRET_KEY environment variable |
| High backend latency | Low cache hit rate | Increase TTL or implement prefetch |

## References

- [HashiCorp Vault Documentation](https://www.vaultproject.io/docs)
- [AWS Secrets Manager Documentation](https://docs.aws.amazon.com/secretsmanager/)
- [Erlang crypto Module](https://erlang.org/doc/man/crypto.html)
- [NIST AES Guidelines](https://csrc.nist.gov/publications/detail/fips/197/final)
- `/Users/sac/erlmcp/docs/SECRETS_MANAGEMENT.md` - Detailed configuration guide
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_secrets.erl` - Implementation
