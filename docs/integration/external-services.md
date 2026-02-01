# External Services Integration

**Version**: 2.1.0
**Last Updated**: 2026-01-31

---

## Overview

This guide covers integrating erlmcp with external cloud services, including Google Cloud Platform (GCP), Amazon Web Services (AWS), Microsoft Azure, HashiCorp Vault, OpenTelemetry backends, and CI/CD platforms. Each integration includes detailed architecture diagrams, configuration examples, and best practices.

## Table of Contents

- [Google Cloud Platform Integration](#google-cloud-platform-integration)
- [AWS Services Integration](#aws-services-integration)
- [Azure Integration](#azure-integration)
- [HashiCorp Vault Integration](#hashicorp-vault-integration)
- [OpenTelemetry Backends](#opentelemetry-backends)
- [CI/CD Platform Integration](#cicd-platform-integration)

---

## Google Cloud Platform Integration

### Architecture Overview

```mermaid
graph TB
    subgraph "erlmcp Cluster"
        MCP[erlmcp Server<br/>3 nodes]
        GCP_INT[gcp_integration<br/>module]
    end

    subgraph "Google Cloud Platform"
        COMPUTE[Compute Engine<br/>VM Instances]
        STORAGE[Cloud Storage<br/>Object Storage]
        PUBSUB[Pub/Sub<br/>Messaging]
        FUNCTIONS[Cloud Functions<br/>Serverless]
        SQL[Cloud SQL<br/>Managed Databases]
        IAM[IAM & Security<br/>Auth & Policies]
    end

    subgraph "Authentication"
        SA_KEY[Service Account<br/>Key File]
        ADC[Application Default<br/>Credentials]
        IMDS[Instance Metadata<br/>Service]
    end

    %% Integration flows
    MCP --> GCP_INT

    GCP_INT -->|REST/gRPC| COMPUTE
    GCP_INT -->|REST/gRPC| STORAGE
    GCP_INT -->|REST/gRPC| PUBSUB
    GCP_INT -->|REST/gRPC| FUNCTIONS
    GCP_INT -->|Cloud SQL Proxy| SQL
    GCP_INT -->|OAuth 2.0| IAM

    %% Authentication flows
    SA_KEY -->|JSON Key| GCP_INT
    ADC -->|Auto-discovery| GCP_INT
    IMDS -->|On GCE| GCP_INT

    style MCP fill:#e1f5e1
    style GCP_INT fill:#ffe1e1
    style SA_KEY fill:#fff5e1
```

### Configuration

**Service Account Authentication:**
```erlang
% config/sys.config
{erlmcp, [
    {gcp_config, #{
        project_id => <<"my-project">>,
        credentials => {service_account, #{
            key_file => <<"/path/to/key.json">>,
            scope => [<<"https://www.googleapis.com/auth/cloud-platform">>]
        }},
        services => [
            compute_engine,
            cloud_storage,
            pubsub,
            cloud_sql
        ],
        timeout_ms => 30000,
        max_retries => 3
    }}
]}.
```

**Application Default Credentials (GCE):**
```erlang
% Auto-discovery on Compute Engine
{gcp_config, #{
    project_id => <<"my-project">>,
    credentials => application_default,
    services => [compute_engine, cloud_storage]
}}.
```

### Integration Example: Compute Engine

```erlang
%% Start GCP-enabled server
{ok, Server} = erlmcp_server:start_link(#{
    gcp_config => #{
        project_id => <<"my-project">>,
        credentials => {service_account, <<"/path/to/key.json">>},
        services => [compute_engine]
    }
}).

%% Compute Engine tools automatically registered
%% Available tools:
%% - gcp_compute_create_instance
%% - gcp_compute_list_instances
%% - gcp_compute_get_instance
%% - gcp_compute_start_instance
%% - gcp_compute_stop_instance
%% - gcp_compute_delete_instance

%% Example: Create VM instance
Request = #{
    jsonrpc => "2.0",
    id => <<"create-vm-001">>,
    method => "tools/call",
    params => #{
        name => "gcp_compute_create_instance",
        arguments => #{
            name => <<"my-instance">>,
            zone => <<"us-central1-a">>,
            machine_type => <<"e2-medium">>,
            image => <<"projects/debian-cloud/global/images/family/debian-11">>
        }
    }
},

%% Result: VM created, instance ID returned
```

### GCP Services Support Matrix

| Service | Integration Type | Status | Example Usage |
|---------|-----------------|--------|---------------|
| **Compute Engine** | REST API | ✅ Full | Create/manage VMs |
| **Cloud Storage** | REST API | ✅ Full | Upload/download objects |
| **Pub/Sub** | REST API | ✅ Full | Publish/subscribe messages |
| **Cloud Functions** | REST API | ✅ Full | Deploy/invoke functions |
| **Cloud SQL** | Proxy | ✅ Full | Query databases |
| **BigQuery** | REST API | 🚧 Planned | Run SQL queries |
| **Cloud Tasks** | REST API | 🚧 Planned | Async task queues |
| **Secret Manager** | REST API | 🚧 Planned | Store secrets |

---

## AWS Services Integration

### Architecture Overview

```mermaid
graph TB
    subgraph "erlmcp Cluster"
        MCP[erlmcp Server]
        AWS_INT[aws_integration<br/>module]
    end

    subgraph "Amazon Web Services"
        LAMBDA[AWS Lambda<br/>Serverless Functions]
        S3[S3<br/>Object Storage]
        SQS[SQS<br/>Message Queue]
        API_GW[API Gateway<br/>REST APIs]
        DYNAMODB[DynamoDB<br/>NoSQL Database]
        IAM[AWS IAM<br/>Access Management]
    end

    subgraph "Authentication"
        AKSK[Access Key<br/>+ Secret Key]
        ROLE[IAM Role<br/>+ STS]
        PROFILE[Named Profile<br/>~/.aws/credentials]
    end

    %% Integration flows
    MCP --> AWS_INT

    AWS_INT -->|SDK/API| LAMBDA
    AWS_INT -->|SDK/API| S3
    AWS_INT -->|SDK/API| SQS
    AWS_INT -->|SDK/API| API_GW
    AWS_INT -->|SDK/API| DYNAMODB
    AWS_INT -->|SIGv4| IAM

    %% Authentication flows
    AKSK -->|Credentials| AWS_INT
    ROLE -->|STS Assume| AWS_INT
    PROFILE -->|Profile| AWS_INT

    style MCP fill:#e1f5e1
    style AWS_INT fill:#ffe1e1
```

### Configuration

**Access Key Authentication:**
```erlang
{erlmcp, [
    {aws_config, #{
        region => <<"us-east-1">>,
        credentials => {access_key, #{
            access_key_id => <<"AKIAIOSFODNN7EXAMPLE">>,
            secret_access_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>
        }},
        services => [lambda, s3, sqs, dynamodb],
        timeout_ms => 30000
    }}
]}.
```

**IAM Role (STS):**
```erlang
{aws_config, #{
    region => <<"us-east-1">>,
    credentials => {iam_role, #{
        role_arn => <<"arn:aws:iam::123456789012:role/erlmcp-role">>,
        session_name => <<"erlmcp-session">>,
        external_id => <<"optional-external-id">>
    }},
    services => [lambda, s3]
}}.
```

### Integration Example: Lambda

```erlang
%% Start AWS-enabled server
{ok, Server} = erlmcp_server:start_link(#{
    aws_config => #{
        region => <<"us-east-1">>,
        credentials => {access_key, Credentials},
        services => [lambda]
    }
}).

%% Invoke Lambda function
Request = #{
    jsonrpc => "2.0",
    id => <<"lambda-001">>,
    method => "tools/call",
    params => #{
        name => "aws_lambda_invoke",
        arguments => #{
            function_name => <<"my-function">>,
            payload => #{
                action => <<"process">>,
                data => <<"example">>
            },
            invocation_type => <<"RequestResponse">>
        }
    }
}.

%% Result: Function response with status code
```

---

## Azure Integration

### Architecture Overview

```mermaid
graph LR
    subgraph "erlmcp"
        MCP[erlmcp Server]
        AZ_INT[azure_integration]
    end

    subgraph "Microsoft Azure"
        FUNCTIONS[Azure Functions<br/>Serverless]
        STORAGE[Blob Storage<br/>Object Storage]
        SERVICEBUS[Service Bus<br/>Messaging]
        KEYVAULT[Key Vault<br/>Secrets]
        COSMOSDB[Cosmos DB<br/>NoSQL Database]
    end

    MCP --> AZ_INT
    AZ_INT -->|REST API| FUNCTIONS
    AZ_INT -->|REST API| STORAGE
    AZ_INT -->|REST API| SERVICEBUS
    AZ_INT -->|REST API| KEYVAULT
    AZ_INT -->|REST API| COSMOSDB

    style MCP fill:#e1f5e1
    style AZ_INT fill:#ffe1e1
```

### Configuration

**Service Principal Authentication:**
```erlang
{erlmcp, [
    {azure_config, #{
        subscription_id => <<"12345678-1234-1234-1234-123456789012">>,
        tenant_id => <<"87654321-4321-4321-4321-210987654321">>,
        client_id => <<"11111111-2222-3333-4444-555555555555">>,
        client_secret => <<"my-client-secret">>,
        services => [functions, blob_storage, service_bus],
        timeout_ms => 30000
    }}
]}.
```

---

## HashiCorp Vault Integration

### Architecture Overview

```mermaid
sequenceDiagram
    participant App as erlmcp Application
    participant Vault as HashiCorp Vault
    participant Secrets as erlmcp_secrets
    participant Cache as Local Cache

    App->>Secrets: Request Secret
    Secrets->>Cache: Check Cache

    alt Cache Hit
        Cache-->>Secrets: Return Cached Secret
        Secrets-->>App: Secret (from cache)
    else Cache Miss
        Secrets->>Vault: Authenticate (AppRole)
        Vault-->>Secrets: Client Token
        Secrets->>Vault: Read Secret
        Vault-->>Secrets: Secret Value
        Secrets->>Cache: Cache Secret (TTL 5min)
        Secrets-->>App: Secret (from Vault)
    end

    Note over Cache: TTL expires after 5 minutes
    Note over Vault: Audit logging enabled
```

### Configuration

**AppRole Authentication:**
```erlang
{erlmcp, [
    {secrets, [
        {backend, vault},
        {backend_config, #{
            address => "https://vault.example.com:8200",
            auth_method => approle,
            role_id => <<"my-role-id">>,
            secret_id => <<"my-secret-id">>,
            engine => "kv",
            mount => "secret"
        }},
        {ttl_seconds => 300},
        {cache_enabled => true}
    ]}
]}.
```

**Token Authentication:**
```erlang
{secrets, [
    {backend, vault},
    {backend_config, #{
        address => "https://vault.example.com:8200",
        auth_method => token,
        token => {env_var, "VAULT_TOKEN"},
        engine => "kv",
        mount => "secret"
    }},
    {ttl_seconds => 300}
]}.
```

### Usage Example

```erlang
%% Initialize secrets manager
{ok, Pid} = erlmcp_secrets:start_link(config).

%% Read secret
case erlmcp_secrets:get(<<"database/credentials">>) of
    {ok, Secret} ->
        Username = maps:get(<<"username">>, Secret),
        Password = maps:get(<<"password">>, Secret),
        %% Use credentials...
        ok;
    {error, Reason} ->
        logger:error("Failed to read secret: ~p", [Reason])
end.

%% Write secret
ok = erlmcp_secrets:put(<<"api/key">>, #{
    <<"key">> => <<"abc123">>,
    <<"expires">> => <<"2026-12-31">>
}).
```

---

## OpenTelemetry Backends

### Observability Pipeline

```mermaid
graph TB
    subgraph "erlmcp Cluster (3 nodes)"
        N1[Node 1]
        N2[Node 2]
        N3[Node 3]

        OTEL1[erlmcp_otel<br/>Agent]
        OTEL2[erlmcp_otel<br/>Agent]
        OTEL3[erlmcp_otel<br/>Agent]
    end

    subgraph "OpenTelemetry Collectors"
        COLLECTOR[OTel Collector<br/>Aggregation]
    end

    subgraph "Backend Services"
        DATADOG[Datadog<br/>APM + Metrics]
        HONEYCOMB[Honeycomb<br/>Observability]
        JAEGER[Jaeger<br/>Tracing]
        PROMETHEUS[Prometheus<br/>Metrics]
    end

    %% Node to collector
    N1 --> OTEL1
    N2 --> OTEL2
    N3 --> OTEL3

    OTEL1 -->|OTLP/gRPC| COLLECTOR
    OTEL2 -->|OTLP/gRPC| COLLECTOR
    OTEL3 -->|OTLP/gRPC| COLLECTOR

    %% Collector to backends
    COLLECTOR -->|OTLP| DATADOG
    COLLECTOR -->|OTLP| HONEYCOMB
    COLLECTOR -->|Jaeger| JAEGER
    COLLECTOR -->|Remote Write| PROMETHEUS

    style COLLECTOR fill:#e1f5e1
    style DATADOG fill:#ffe1e1
    style HONEYCOMB fill:#ffe1e1
    style JAEGER fill:#ffe1e1
```

### Configuration

**Datadog Exporter:**
```erlang
{erlmcp_observability, [
    {otel, [
        {exporters, [
            {datadog, #{
                endpoint => "https://trace.agent.datadoghq.com:4318",
                api_key => {env_var, "DATADOG_API_KEY"},
                service_name => <<"erlmcp">>,
                env => <<"production">>,
                batch_size => 512,
                timeout_ms => 5000
            }}
        ]},
        {trace_sampling, 1.0},  % 100% sampling
        {metric_export_interval_ms, 10000}
    ]}
]}.
```

**Honeycomb Exporter:**
```erlang
{otel, [
    {exporters, [
        {honeycomb, #{
            endpoint => "https://api.honeycomb.io:443",
            api_key => {env_var, "HONEYCOMB_API_KEY"},
            dataset => <<"erlmcp-prod">>,
            sample_rate => 1
        }}
    ]}
]}.
```

**Jaeger Exporter:**
```erlang
{otel, [
    {exporters, [
        {jaeger, #{
            endpoint => "http://jaeger:14268/api/traces",
            agent_host => "jaeger-agent",
            agent_port => 6831,
            service_name => <<"erlmcp">>
        }}
    ]}
]}.
```

---

## CI/CD Platform Integration

### GitHub Actions Workflow

```yaml
# .github/workflows/erlmcp-ci.yml
name: erlmcp CI/CD

on:
  push:
    branches: [main, develop]
  pull_request:
    branches: [main]

jobs:
  quality-gates:
    runs-on: ubuntu-latest
    container:
      image: ghcr.io/cortexlabs/erlang:25.3

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Compile
        run: rebar3 compile

      - name: Dialyzer
        run: rebar3 dialyzer

      - name: Xref
        run: rebar3 xref

      - name: Unit Tests
        run: rebar3 eunit

      - name: Integration Tests
        run: rebar3 ct

      - name: Coverage
        run: rebar3 cover

      - name: Upload Coverage
        uses: codecov/codecov-action@v3
        with:
          file: _build/test/cover/eunit.coverdata

  deploy:
    needs: quality-gates
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'

    steps:
      - name: Deploy to Production
        run: |
          # Build release
          rebar3 as prod tar

          # Deploy to cluster
          kubectl apply -f k8s/
```

### GitLab CI Pipeline

```yaml
# .gitlab-ci.yml
stages:
  - test
  - build
  - deploy

test:
  stage: test
  image: ghcr.io/cortexlabs/erlang:25.3
  script:
    - rebar3 compile
    - rebar3 eunit
    - rebar3 ct
    - rebar3 cover
  coverage: '/\d+\%\s+covered/'
  artifacts:
    reports:
      coverage_report:
        coverage_format: cobertura
        path: coverage.xml

build:
  stage: build
  image: ghcr.io/cortexlabs/erlang:25.3
  script:
    - rebar3 as prod tar
  artifacts:
    paths:
      - _build/prod/rel/*.tar.gz
  only:
    - main

deploy:
  stage: deploy
  image: bitnami/kubectl:latest
  script:
    - kubectl set image deployment/erlmcp erlmcp=$CI_REGISTRY_IMAGE:$CI_COMMIT_SHA
    - kubectl rollout status deployment/erlmcp
  only:
    - main
```

---

## Best Practices

### 1. Authentication & Security

```mermaid
graph TB
    subgraph "Security Layers"
        L1[Layer 1: Network Security<br/>mTLS, VPN]
        L2[Layer 2: Service Authentication<br/>OAuth 2.0, JWT]
        L3[Layer 3: Secrets Management<br/>Vault, AWS SM]
        L4[Layer 4: Authorization<br/>IAM, RBAC]
    end

    L1 --> L2
    L2 --> L3
    L3 --> L4

    style L1 fill:#e1f5e1
    style L2 fill:#ffe1e1
    style L3 fill:#fff5e1
    style L4 fill:#e1e1ff
```

**Best Practices:**
- ✅ Use workload identity federation (no long-lived keys)
- ✅ Rotate credentials automatically (Vault dynamic secrets)
- ✅ Enable audit logging for all secret access
- ✅ Use least-privilege IAM roles
- ✅ Encrypt secrets at rest and in transit

### 2. Resilience & Fault Tolerance

**Retry Strategy:**
```erlang
%% Exponential backoff with jitter
retry_strategy() ->
    #{
        max_retries => 3,
        initial_delay_ms => 1000,
        max_delay_ms => 10000,
        multiplier => 2.0,
        jitter => 0.1  % Add 10% random jitter
    }.
```

**Circuit Breaker:**
```erlang
%% Configure circuit breaker for external service
{circuit_breaker, #{
    threshold => 5,          % Open after 5 failures
    timeout_ms => 60000,     % Try again after 60s
    half_open_max_calls => 3 % Try 3 calls in half-open
}}.
```

### 3. Cost Optimization

| Service | Cost Optimization | Savings |
|---------|------------------|---------|
| **GCP Compute Engine** | Use preemptible VMs | 60-80% |
| **AWS Lambda** | Right-size memory, use provisioned concurrency | 40-60% |
| **Azure Functions** | Use premium plan for high throughput | 30-50% |
| **Cloud Storage/S3** | Use lifecycle policies, transition to cold storage | 50-70% |

---

## Troubleshooting

### Common Issues

**Issue 1: Authentication Failed**
```
Error: {error, unauthorized}
Cause: Invalid credentials or expired token
Solution:
1. Check service account key: gcloud auth activate-service-account --key-file=key.json
2. Verify IAM permissions: gcloud projects get-iam-policy PROJECT_ID
3. Check token expiration: gcloud auth print-access-token
```

**Issue 2: Rate Limiting**
```
Error: {error, rate_limited}
Cause: Too many API requests
Solution:
1. Implement exponential backoff
2. Use batch APIs where available
3. Request quota increase from cloud provider
```

**Issue 3: Network Timeout**
```
Error: {error, timeout}
Cause: Network latency or service unresponsive
Solution:
1. Increase timeout in config
2. Check VPC connectivity
3. Enable VPC peering for private endpoints
```

---

## Next Steps

- [Database Integration Guide](./database.md) - Data persistence strategies
- [API Gateway Guide](./api-gateway.md) - Load balancing patterns
- [Integration Patterns Reference](../archive/misc/INTEGRATION_PATTERNS.md)

---

**Version**: 2.1.0
**Last Updated**: 2026-01-31
