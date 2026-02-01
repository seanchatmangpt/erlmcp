# Integration Architecture

**Version**: 2.1.0
**Last Updated**: 2026-01-31

---

## Overview

This document describes the complete integration architecture of erlmcp, including system topology, component relationships, data flow patterns, service mesh integration, supervision tree integration, and module dependencies. It provides the foundation for understanding how to integrate erlmcp with external systems.

## Table of Contents

- [System Architecture Overview](#system-architecture-overview)
- [Component Integration](#component-integration)
- [Data Flow Patterns](#data-flow-patterns)
- [Service Mesh Integration](#service-mesh-integration)
- [Supervision Tree Integration](#supervision-tree-integration)
- [Transport Layer Integration](#transport-layer-integration)
- [Module Dependency Analysis](#module-dependency-analysis)
- [Integration Points](#integration-points)
- [Scalability Patterns](#scalability-patterns)

---

## System Architecture Overview

### Complete System Topology

```mermaid
graph TB
    subgraph "External Systems"
        AI_CLIENTS[AI Runtimes<br/>Claude, GPT-4, Local LLMs]
        DEV_TOOLS[Developer Tools<br/>VSCode, JetBrains, CLI]
        CLOUD_SERVICES[Cloud Services<br/>GCP, AWS, Azure]
        MONITORING[Monitoring Systems<br/>Datadog, Honeycomb, Jaeger]
        CI_CD[CI/CD Platforms<br/>GitHub Actions, GitLab CI]
    end

    subgraph "API Gateway Layer"
        NGINX[Nginx/Gateway<br/>Load Balancer]
        K8S_INGRESS[Kubernetes Ingress<br/>Service Mesh]
    end

    subgraph "erlmcp Cluster"
        direction TB

        subgraph "Node 1 (Primary)"
            N1_CORE[Core App<br/>97 modules]
            N1_TRANS[Transports App<br/>23 modules]
            N1_OBS[Observability App<br/>31 modules]
            N1_VAL[Validation App<br/>13 modules]
        end

        subgraph "Node 2 (Replica)"
            N2_CORE[Core App<br/>97 modules]
            N2_TRANS[Transports App<br/>23 modules]
            N2_OBS[Observability App<br/>31 modules]
            N2_VAL[Validation App<br/>13 modules]
        end

        subgraph "Node 3 (Replica)"
            N3_CORE[Core App<br/>97 modules]
            N3_TRANS[Transports App<br/>23 modules]
            N3_OBS[Observability App<br/>31 modules]
            N3_VAL[Validation App<br/>13 modules]
        end

        subgraph "Shared Storage"
            Mnesia[Mnesia Cluster<br/>Distributed DB]
            Registry[gproc Registry<br/>Distributed Registry]
        end
    end

    subgraph "External Dependencies"
        GPROC[gproc<br/>Process Registry]
        JSX[jsx<br/>JSON Codec]
        JESSE[jesse<br/>JSON Schema]
        GUN[gun<br/>HTTP Client]
        RANCH[ranch<br/>TCP Acceptors]
        COWBOY[cowboy<br/>HTTP Server]
        POOLBOY[poolboy<br/>Pool Mgmt]
    end

    %% External connections
    AI_CLIENTS --> NGINX
    DEV_TOOLS --> NGINX
    NGINX --> K8S_INGRESS
    CLOUD_SERVICES -.->|API Calls| N1_CORE
    MONITORING -.->|OTLP| N1_OBS
    CI_CD -.->|Deploy| N1_VAL

    %% Gateway to cluster
    K8S_INGRESS --> N1_TRANS
    K8S_INGRESS --> N2_TRANS
    K8S_INGRESS --> N3_TRANS

    %% Inter-node communication
    N1_CORE <--> Mnesia
    N2_CORE <--> Mnesia
    N3_CORE <--> Mnesia

    N1_CORE <--> Registry
    N2_CORE <--> Registry
    N3_CORE <--> Registry

    %% Internal app connections
    N1_TRANS --> N1_CORE
    N1_CORE --> N1_OBS
    N1_CORE --> N1_VAL

    N2_TRANS --> N2_CORE
    N2_CORE --> N2_OBS
    N2_CORE --> N2_VAL

    N3_TRANS --> N3_CORE
    N3_CORE --> N3_OBS
    N3_CORE --> N3_VAL

    %% External library connections
    N1_TRANS --> RANCH
    N1_TRANS --> COWBOY
    N1_TRANS --> GUN
    N1_CORE --> GPROC
    N1_CORE --> JSX
    N1_CORE --> JESSE
    N1_CORE --> POOLBOY

    style N1_CORE fill:#e1f5e1
    style N2_CORE fill:#e1f5e1
    style N3_CORE fill:#e1f5e1
    style Mnesia fill:#ffe1e1
    style Registry fill:#ffe1e1
```

**Architecture Principles:**
1. **Horizontal Scaling** - Add nodes for increased capacity
2. **Fault Isolation** - Node failures don't affect other nodes
3. **Data Consistency** - Mnesia provides distributed transactions
4. **Service Discovery** - gproc enables dynamic routing
5. **Observability** - All nodes export telemetry

---

## Component Integration

### Layer Breakdown

```mermaid
graph TB
    subgraph "Application Stack (164 modules)"
        direction TB

        Layer1[Layer 1: Validation<br/>13 modules<br/>Compliance & Quality Gates]
        Layer2[Layer 2: Core<br/>97 modules<br/>Protocol & Capabilities]
        Layer3[Layer 3: Transports<br/>23 modules<br/>I/O & Communication]
        Layer4[Layer 4: Observability<br/>31 modules<br/>Metrics & Tracing]
    end

    subgraph "External Integration Points"
        direction LR

        E1[External AI Runtimes]
        E2[Cloud Services]
        E3[Monitoring Systems]
        E4[CI/CD Platforms]
    end

    %% Integration connections
    E1 --> Layer3
    E2 -.-> Layer2
    E3 -.-> Layer4
    E4 -.-> Layer1

    %% Internal layer connections
    Layer1 -.->|Validates| Layer2
    Layer2 -->|Uses| Layer3
    Layer2 -->|Emits| Layer4
    Layer3 -->|Transports| Layer4
    Layer4 -->|Monitors| Layer2

    style Layer1 fill:#e1f5ff
    style Layer2 fill:#e1ffe1
    style Layer3 fill:#ffe1e1
    style Layer4 fill:#fff5e1
```

**Layer Responsibilities:**

| Layer | Modules | Responsibility | External Integration |
|-------|---------|----------------|----------------------|
| **Validation** | 13 | Compliance, testing, quality gates | CI/CD platforms, test runners |
| **Core** | 97 | MCP protocol, capabilities, session management | Cloud services, databases |
| **Transports** | 23 | stdio, TCP, HTTP, WebSocket, SSE | AI runtimes, load balancers |
| **Observability** | 31 | Metrics, tracing, chaos engineering | Monitoring systems, APM tools |

---

## Data Flow Patterns

### Request-Response Flow

```mermaid
sequenceDiagram
    participant AI as AI Runtime
    participant GW as API Gateway
    participant Trans as Transport Layer
    participant Core as Core Layer
    participant Obs as Observability
    participant Ext as External Services

    AI->>GW: JSON-RPC Request
    GW->>Trans: Route to node

    Trans->>Core: Parse Message
    activate Core
    Core->>Obs: Start Span
    Obs-->>Core: trace_id

    Core->>Core: Validate Request
    Core->>Core: Check Capabilities
    Core->>Core: Execute Handler

    alt External Service Call
        Core->>Ext: API Request
        Ext-->>Core: Response
    end

    Core->>Obs: Record Metrics
    Core->>Obs: End Span

    Core->>Trans: JSON-RPC Response
    deactivate Core
    Trans->>GW: Response
    GW->>AI: JSON-RPC Response
```

### Streaming Data Flow

```mermaid
sequenceDiagram
    participant Client as MCP Client
    participant Server as erlmcp Server
    participant Stream as Stream Handler
    participant Resource as Resource Backend

    Client->>Server: Subscribe to Resource
    Server->>Resource: Register Subscription

    loop Change Notifications
        Resource->>Server: Resource Changed
        Server->>Stream: Prepare Notification
        Stream->>Client: SSE/WS Event
        Client->>Client: Process Update
    end

    Client->>Server: Unsubscribe
    Server->>Resource: Remove Subscription
```

---

## Service Mesh Integration

### Istio/Linkerd Integration Pattern

```mermaid
graph TB
    subgraph "Service Mesh (Istio)"
        INGRESS[Istio Ingress Gateway]
        PILOT[Istio Pilot<br/>Service Discovery]
        CITADEL[Citadel<br/>mTLS]
        GALLEY[Galley<br/>Config Validation]
    end

    subgraph "erlmcp Namespace"
        POD1[erlmcp-1<br/>Pod]
        POD2[erlmcp-2<br/>Pod]
        POD3[erlmcp-3<br/>Pod]

        SIDECAR1[Envoy<br/>Sidecar]
        SIDECAR2[Envoy<br/>Sidecar]
        SIDECAR3[Envoy<br/>Sidecar]
    end

    subgraph "External Services"
        GCP[GCP Services]
        AWS[AWS Services]
        MONITOR[Datadog/Jaeger]
    end

    %% Mesh connections
    INGRESS --> PILOT
    PILOT --> POD1
    PILOT --> POD2
    PILOT --> POD3

    POD1 --> SIDECAR1
    POD2 --> SIDECAR2
    POD3 --> SIDECAR3

    %% mTLS
    CITADEL -.->|mTLS Certs| SIDECAR1
    CITADEL -.->|mTLS Certs| SIDECAR2
    CITADEL -.->|mTLS Certs| SIDECAR3

    %% External calls
    SIDECAR1 -.->|via mesh| GCP
    SIDECAR2 -.->|via mesh| AWS
    SIDECAR3 -.->|OTLP| MONITOR

    %% Inter-pod communication
    SIDECAR1 <--> SIDECAR2
    SIDECAR2 <--> SIDECAR3

    style INGRESS fill:#e1f5e1
    style PILOT fill:#ffe1e1
    style CITADEL fill:#ffe1e1
```

**Service Mesh Features:**
- **mTLS** - Automatic encryption between all services
- **Traffic Management** - Canary deployments, A/B testing
- **Observability** - Distributed tracing out of the box
- **Policy Enforcement** - Rate limiting, access control

**Configuration:**
```yaml
# Istio ServiceEntry for erlmcp
apiVersion: networking.istio.io/v1beta1
kind: ServiceEntry
metadata:
  name: erlmcp
spec:
  hosts:
  - erlmcp.default.svc.cluster.local
  ports:
  - number: 8080
    name: tcp
    protocol: TCP
  location: MESH_INTERNAL
  resolution: DNS
```

---

## Supervision Tree Integration

### Three-Tier Supervision Hierarchy

```mermaid
graph TB
    subgraph "Tier 1: Application Supervisors (one_for_all)"
        direction TB

        CORE_SUP[erlmcp_core_sup]
        TRANS_SUP[erlmcp_transports_sup]
        OBS_SUP[erlmcp_observability_sup]
        VAL_SUP[erlmcp_validation_sup]
    end

    subgraph "Tier 2: Service Supervisors (simple_one_for_one)"
        direction TB

        SERVER_SUP[erlmcp_server_sup<br/>Dynamic Server Workers]
        CLIENT_SUP[erlmcp_client_sup<br/>Dynamic Client Workers]
        SESSION_MGR[erlmcp_session_manager<br/>Session Lifecycle]
        TRANSPORT_POOL[erlmcp_transport_pool<br/>Connection Pooling]
    end

    subgraph "Tier 3: Worker Processes (isolated)"
        direction TB

        SERVER_WORKERS[Server Workers<br/>per-connection processes]
        CLIENT_WORKERS[Client Workers<br/>per-request processes]
        SESSION_WORKERS[Session Workers<br/>per-session processes]
        OTEL_EXPORTERS[OTEL Exporters<br/>isolated telemetry]
    end

    %% Tier 1 to Tier 2
    CORE_SUP --> SERVER_SUP
    CORE_SUP --> CLIENT_SUP
    CORE_SUP --> SESSION_MGR

    TRANS_SUP --> TRANSPORT_POOL

    OBS_SUP --> OTEL_EXPORTERS

    %% Tier 2 to Tier 3
    SERVER_SUP --> SERVER_WORKERS
    CLIENT_SUP --> CLIENT_WORKERS
    SESSION_MGR --> SESSION_WORKERS

    %% Infrastructure processes (direct children of Tier 1)
    CORE_SUP --> REGISTRY[erlmcp_registry<br/>gproc]
    CORE_SUP --> TASK_MGR[erlmcp_task_manager<br/>Task State Machine]
    TRANS_SUP --> TRANSPORT_HEALTH[erlmcp_transport_health<br/>Health Monitor]

    style CORE_SUP fill:#e1f5e1
    style TRANS_SUP fill:#e1f5e1
    style OBS_SUP fill:#e1f5e1
    style VAL_SUP fill:#e1f5e1
    style SERVER_SUP fill:#ffe1e1
    style CLIENT_SUP fill:#ffe1e1
    style SESSION_MGR fill:#ffe1e1
```

**Supervision Strategies:**

| Tier | Strategy | Use Case | Failure Impact |
|------|----------|----------|----------------|
| **Tier 1** | `one_for_all` | Application supervisors | All children restart if one fails |
| **Tier 2** | `simple_one_for_one` | Dynamic workers (servers, clients) | Only failed worker restarts |
| **Tier 3** | `isolated` | Observability exporters | No cascade failures |

---

## Transport Layer Integration

### Transport Polymorphism

```mermaid
classDiagram
    class erlmcp_transport {
        <<behavior>>
        init/2
        send/2
        close/1
        handle_info/2
    }

    class erlmcp_transport_stdio {
        init/2
        send/2
        close/1
        handle_info/2
        stdin::port()
        stdout::port()
    }

    class erlmcp_transport_tcp {
        init/2
        send/2
        close/1
        handle_info/2
        ranch::ref()
        socket::port()
    }

    class erlmcp_transport_http {
        init/2
        send/2
        close/1
        handle_info/2
        gun::conn()
        stream_ref::ref()
    }

    class erlmcp_transport_ws {
        init/2
        send/2
        close/1
        handle_info/2
        gun::ws_conn()
        ws_path::binary()
    }

    class erlmcp_transport_sse {
        init/2
        send/2
        close/1
        handle_info/2
        cowboy::req()
        event_stream::pid()
    }

    erlmcp_transport <|-- erlmcp_transport_stdio
    erlmcp_transport <|-- erlmcp_transport_tcp
    erlmcp_transport <|-- erlmcp_transport_http
    erlmcp_transport <|-- erlmcp_transport_ws
    erlmcp_transport <|-- erlmcp_transport_sse

    class erlmcp_server {
        handle_transport/2
        transport_module::atom()
        transport_state::map()
    }

    erlmcp_server --> erlmcp_transport : uses
```

**Transport Selection Guide:**

| Transport | Best For | Max Connections | Latency | Bidirectional |
|-----------|----------|-----------------|---------|---------------|
| **stdio** | Local AI runtimes | 1 | <1ms | No |
| **TCP** | High-performance | 50K/node | 1-5ms | Yes |
| **HTTP** | REST APIs | 10K/node | 5-20ms | Request/Response |
| **WebSocket** | Real-time | 5K/node | <5ms | Yes |
| **SSE** | Notifications | 10K/node | <10ms | Server→Client |

---

## Module Dependency Analysis

### Core Dependencies

```mermaid
graph LR
    subgraph "Core Modules"
        SERVER[erlmcp_server]
        CLIENT[erlmcp_client]
        REGISTRY[erlmcp_registry]
        JSON_RPC[erlmcp_json_rpc]
        MSG_HANDLER[erlmcp_message_handler]
    end

    subgraph "Capability Modules"
        TOOLS[erlmcp_tool]
        RESOURCES[erlmcp_resources]
        PROMPTS[erlmcp_prompt_template]
        TASKS[erlmcp_task_manager]
    end

    subgraph "Security Modules"
        AUTH[erlmcp_auth]
        SECRETS[erlmcp_secrets]
        RATE_LIMIT[erlmcp_rate_limiter]
    end

    subgraph "Session Modules"
        SESSION[erlmcp_session]
        SESSION_MGR[erlmcp_session_manager]
        SESSION_BACKENDS[Session Backends]
    end

    %% Core dependencies
    SERVER --> REGISTRY
    SERVER --> JSON_RPC
    SERVER --> MSG_HANDLER
    CLIENT --> JSON_RPC

    %% Capability dependencies
    SERVER --> TOOLS
    SERVER --> RESOURCES
    SERVER --> PROMPTS
    SERVER --> TASKS

    %% Security dependencies
    SERVER --> AUTH
    AUTH --> SECRETS
    SERVER --> RATE_LIMIT

    %% Session dependencies
    SERVER --> SESSION
    SESSION --> SESSION_BACKENDS
    CLIENT --> SESSION_MGR
    SESSION_MGR --> SESSION_BACKENDS

    style SERVER fill:#e1f5e1
    style CLIENT fill:#e1f5e1
    style REGISTRY fill:#ffe1e1
```

**Dependency Categories:**

1. **Critical Path** - Required for basic operation
   - erlmcp_server, erlmcp_client, erlmcp_registry, erlmcp_json_rpc

2. **Capability Path** - Required for MCP features
   - erlmcp_tool, erlmcp_resources, erlmcp_prompt_template, erlmcp_task_manager

3. **Security Path** - Required for production deployments
   - erlmcp_auth, erlmcp_secrets, erlmcp_rate_limiter

4. **Session Path** - Required for stateful connections
   - erlmcp_session, erlmcp_session_manager, session backends

---

## Integration Points

### External System Hooks

```mermaid
mindmap
  root((erlmcp<br/>Integration Points))
    Transport Layer
      stdio:::stdio
        Local AI runtimes
        Process I/O
      TCP:::tcp
        Network services
        Load balancers
      HTTP:::http
        REST APIs
        Webhooks
      WebSocket:::ws
        Real-time updates
        Browser clients
      SSE:::sse
        Notifications
        Live updates
    Cloud Services
      GCP
        Compute Engine
        Cloud Storage
        Pub/Sub
      AWS
        Lambda
        S3
        SQS
      Azure
        Functions
        Blob Storage
        Service Bus
    Observability
      Metrics
        Datadog
        Prometheus
        CloudWatch
      Tracing
        Jaeger
        Honeycomb
        X-Ray
      Logging
        ELK Stack
        Cloud Logging
    Data Storage
      ETS
        In-memory cache
      DETS
        Disk persistence
      Mnesia
        Distributed DB
      Cloud SQL
        PostgreSQL
        MySQL
    Security
      Authentication
        OAuth 2.0
        JWT
        mTLS
      Secrets
        Vault
        AWS Secrets Manager
        Azure Key Vault
    CI/CD
      GitHub Actions
      GitLab CI
      Jenkins
      ArgoCD
```

---

## Scalability Patterns

### Scaling Strategies

```mermaid
graph TB
    subgraph "Vertical Scaling (Single Node)"
        V1[Increase CPU]
        V2[Increase Memory]
        V3[Optimize Code]
    end

    subgraph "Horizontal Scaling (Cluster)"
        direction LR

        H1[Node 1<br/>50K connections]
        H2[Node 2<br/>50K connections]
        H3[Node 3<br/>50K connections]
        H4[Node N<br/>50K connections]

        LB[Load Balancer<br/>nginx/HAProxy/K8s]
    end

    subgraph "Data Partitioning"
        P1[Partition 1<br/>Sessions A-F]
        P2[Partition 2<br/>Sessions G-L]
        P3[Partition 3<br/>Sessions M-R]
        P4[Partition 4<br/>Sessions S-Z]
    end

    %% Horizontal scaling
    LB --> H1
    LB --> H2
    LB --> H3
    LB --> H4

    %% Data partitioning
    H1 --> P1
    H2 --> P2
    H3 --> P3
    H4 --> P4

    %% Inter-node communication
    H1 <--> H2
    H2 <--> H3
    H3 <--> H4

    style LB fill:#e1f5e1
```

**Scaling Guidelines:**

| Scale | Connections | Nodes | Transport | Session Backend |
|-------|-------------|-------|-----------|-----------------|
| **Small** | <1K | 1 | stdio, TCP | ETS |
| **Medium** | 1K-50K | 1-3 | TCP, HTTP | ETS, DETS |
| **Large** | 50K-500K | 3-10 | TCP, WS | Mnesia |
| **X-Large** | >500K | 10+ | TCP, WS | Mnesia + Partitioning |

---

## Next Steps

- **External Services Integration** - [Read guide](./external-services.md)
- **Database Integration** - [Read guide](./database.md)
- **API Gateway** - [Read guide](./api-gateway.md)
- **Integration Patterns** - [Read reference](../archive/misc/INTEGRATION_PATTERNS.md)

---

**Version**: 2.1.0
**Last Updated**: 2026-01-31
