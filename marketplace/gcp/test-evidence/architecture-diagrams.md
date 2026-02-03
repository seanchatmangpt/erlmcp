# erlmcp GCP Marketplace Architecture Diagrams

## Document Reference
This document contains text-based architecture diagrams referenced in the main architecture assessment.

---

## 1. GKE Regional Architecture (Recommended for Production)

```
                    GCP Region: us-central1
    ┌─────────────────────────────────────────────────────────────────┐
    │                     Regional GKE Cluster                        │
    │  ┌───────────────────────────────────────────────────────────┐  │
    │  │              Private Control Plane (No Public IP)          │  │
    │  │              CIDR: 172.16.0.0/28                          │  │
    │  │              SLA: 99.95% (21.6 min/month downtime)        │  │
    │  └───────────────────────────────────────────────────────────┘  │
    │                                                                  │
    │  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────┐  │
    │  │   Zone A         │  │   Zone B         │  │   Zone C     │  │
    │  │  us-central1-a   │  │  us-central1-b   │  │ us-central1-c│  │
    │  │                  │  │                  │  │              │  │
    │  │  ┌────────────┐  │  │  ┌────────────┐  │  │ ┌────────┐  │  │
    │  │  │Primary     │  │  │  │Primary     │  │  │ │Primary │  │  │
    │  │  │Node Pool   │  │  │  │Node Pool   │  │  │ │Node    │  │  │
    │  │  │3x e2-std-4 │  │  │  │3x e2-std-4 │  │  │ │Pool    │  │  │
    │  │  │Shielded    │  │  │  │Shielded    │  │  │ │3x e2-  │  │  │
    │  │  │Secure Boot │  │  │  │Secure Boot │  │  │ │std-4   │  │  │
    │  │  └────────────┘  │  │  └────────────┘  │  │ └────────┘  │  │
    │  │  ┌────────────┐  │  │  ┌────────────┐  │  │              │  │
    │  │  │Spot Pool   │  │  │  │Spot Pool   │  │  │              │  │
    │  │  │0-5x e2-std-2│  │  │  │0-5x e2-std-2│  │  │              │  │
    │  │  │Preemptible │  │  │  │Preemptible │  │  │              │  │
    │  │  └────────────┘  │  │  └────────────┘  │  │              │  │
    │  └──────────────────┘  └──────────────────┘  └──────────────┘  │
    └─────────────────────────────────────────────────────────────────┘
                                  │
                                  ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                   VPC Network (erlmcp-vpc)                      │
    │  ┌──────────────────────────────────────────────────────────┐  │
    │  │  Subnet: 10.0.0.0/24 (us-central1)                       │  │
    │  │    Secondary - Pods: 10.1.0.0/16                         │  │
    │  │    Secondary - Services: 10.2.0.0/16                     │  │
    │  └──────────────────────────────────────────────────────────┘  │
    │  ┌──────────────────────────────────────────────────────────┐  │
    │  │  Cloud Router + Cloud NAT (Private egress)                │  │
    │  └──────────────────────────────────────────────────────────┘  │
    └─────────────────────────────────────────────────────────────────┘
                                  │
                    ┌───────────────┼───────────────┐
                    ▼               ▼               ▼
    ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
    │  Secret Manager │  │  Cloud Logging  │  │ Cloud Monitoring │
    │  (11 secrets)   │  │  (aggregated)   │  │ (Prometheus +    │
    │                 │  │                 │  │  custom metrics) │
    └─────────────────┘  └─────────────────┘  └─────────────────┘
```

**Key Configuration:**
- **Release Channel:** REGULAR (balanced stability/features)
- **Datapath:** ADVANCED_DATAPATH V2
- **Network Policy:** Calico for zero-trust
- **Autoscaling:** BALANCED profile, 3-10 nodes
- **Pod Disruption Budget:** minAvailable: 1

---

## 2. Cloud Run Serverless Architecture

```
                    GCP Region: us-central1
    ┌─────────────────────────────────────────────────────────────────┐
    │                     Cloud Run Service                            │
    │  ┌───────────────────────────────────────────────────────────┐  │
    │  │              Revision Controller                           │  │
    │  │                                                           │  │
    │  │  ┌─────────┐    ┌─────────┐    ┌─────────┐              │  │
    │  │  │ Latest  │    │ Traffic │    │ Canary  │              │  │
    │  │  │ Revision│◄───┤ Split  │◄───┤ (5%)    │              │  │
    │  │  │ 100%    │    │         │    │         │              │  │
    │  │  └─────────┘    └─────────┘    └─────────┘              │  │
    │  └───────────────────────────────────────────────────────────┘  │
    │                                                                  │
    │  ┌───────────────────────────────────────────────────────────┐  │
    │  │              Auto-scaling Configuration                    │  │
    │  │  Min Instances: 0 (scale to zero)                          │  │
    │  │  Max Instances: 100                                        │  │
    │  │  Target Concurrency: 80 requests/instance                  │  │
    │  └───────────────────────────────────────────────────────────┘  │
    │                                                                  │
    │  ┌───────────────────────────────────────────────────────────┐  │
    │  │              Container Instance                             │  │
    │  │  ┌─────────────────────────────────────────────────────┐  │  │
    │  │  │  CPU: 1 vCPU                                         │  │  │
    │  │  │  Memory: 512 MiB                                     │  │  │
    │  │  │  Timeout: 300 seconds                                 │  │  │
    │  │  │  Concurrency: 80 requests                             │  │  │
    │  │  └─────────────────────────────────────────────────────┘  │  │
    │  └───────────────────────────────────────────────────────────┘  │
    └─────────────────────────────────────────────────────────────────┘
                                  │
                                  ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                      Load Balancer                              │
    │  - Cloud Run ingress (global anycast IP)                       │
    │  - Cloud CDN integration (optional)                            │
    │  - Cloud Armor (DDoS protection)                               │
    └─────────────────────────────────────────────────────────────────┘
                                  │
                    ┌───────────────┼───────────────┐
                    ▼               ▼               ▼
    ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
    │  Secret Manager │  │  Cloud Logging  │  │ Cloud Monitoring │
    │  (via env vars) │  │  (automatic)    │  │ (automatic)      │
    └─────────────────┘  └─────────────────┘  └─────────────────┘
```

**Scaling Formula:**
```
Instances = ceil(Requests / Target Concurrency)
Example: 1000 requests / 80 = 13 instances
```

---

## 3. Compute Engine MIG Architecture

```
                    GCP Zone: us-central1-a
    ┌─────────────────────────────────────────────────────────────────┐
    │              Managed Instance Group (MIG)                       │
    │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐         │
    │  │erlmcp-  │  │erlmcp-  │  │erlmcp-  │  │erlmcp-  │  ...    │
    │  │ vm-1    │  │ vm-2    │  │ vm-3    │  │ vm-4    │         │
    │  │e2-std-4 │  │e2-std-4 │  │e2-std-4 │  │e2-std-4 │         │
    │  │Shielded │  │Shielded │  │Shielded │  │Shielded │         │
    │  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘         │
    │       │            │            │            │                │
    └───────┼────────────┼────────────┼────────────┼────────────────┘
            │            │            │            │
            └────────────┴────────────┴────────────┘
                                 │
                                 ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                 Instance Template                               │
    │  - Custom Image: erlmcp-marketplace-v1                          │
    │  - Service Account: erlmcp-vm-sa                                 │
    │  - Scopes: cloud-platform, logging.write, monitoring.write      │
    │  - Metadata: SSH keys, startup script                           │
    │  - Shielded VM: Secure boot + integrity monitoring              │
    └─────────────────────────────────────────────────────────────────┘
                                 │
                                 ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │              Regional Load Balancer (HTTP/S)                     │
    │  - Backend Service: MIG                                         │
    │  - Health Check: GET /health (port 8080)                        │
    │  - Session Affinity: None                                       │
    │  - Timeout: 30 seconds                                          │
    └─────────────────────────────────────────────────────────────────┘
                                 │
                    ┌───────────────┼───────────────┐
                    ▼               ▼               ▼
    ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
    │  Secret Manager │  │  Cloud Logging  │  │ Cloud Monitoring │
    │  (via startup)  │  │  (Ops Agent)    │  │ (Ops Agent)      │
    └─────────────────┘  └─────────────────┘  └─────────────────┘
```

**Auto-scaling Configuration:**
```
Min Instances: 2
Max Instances: 10
Target CPU: 70%
Cooldown: 60 seconds
Health Check: HTTP /health, port 8080
```

---

## 4. Container Internal Architecture

```
    ┌─────────────────────────────────────────────────────────────────┐
    │                      erlmcp Container                            │
    │                                                                   │
    │  User: erlmcp (non-root, UID 1000)                              │
    │  Filesystem: Read-only (except /tmp, /var/run)                   │
    │  Capabilities: ALL dropped                                       │
    │                                                                   │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │                    Application Layer                         │ │
    │  │  ┌────────────┐  ┌────────────┐  ┌──────────────────────┐ │ │
    │  │  │ JSON-RPC   │  │ Transport  │  │  Session Management  │ │ │
    │  │  │ Handler    │  │ Manager    │  │  (gen_statem)        │ │ │
    │  │  │            │  │            │  │                      │ │ │
    │  │  │- Parse     │  │- stdio     │  │- Session state       │ │ │
    │  │  │- Validate  │  │- TCP       │  │- Connection tracking │ │ │
    │  │  │- Execute   │  │- HTTP      │  │- Timeouts            │ │ │
    │  │  │- Respond   │  │- WebSocket │  │- Rate limiting        │ │ │
    │  │  └────────────┘  │- SSE       │  └──────────────────────┘ │ │
    │  │                  └────────────┘                             │ │
    │  └────────────────────────────────────────────────────────────┘ │
    │                              │                                   │
    │                              ▼                                   │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │                    Erlang/OTP Runtime                         │ │
    │  │  ┌────────────┐  ┌────────────┐  ┌──────────────────────┐ │ │
    │  │  │  Process   │  │ Distribution│  │  Supervisor Tree     │ │ │
    │  │  │  Pool      │  │  (inet_tls)│  │  (OTP supervision)    │ │ │
    │  │  │            │  │            │  │                      │ │ │
    │  │  │- Lightweight│  │- TLS 1.3   │  │- One-for-all         │ │ │
    │  │  │- Isolated  │  │- mTLS      │  │- Let-it-crash        │ │ │
    │  │  │- Preemptive│  │- EPMD      │  │- Auto-restart        │ │ │
    │  │  │ scheduling │  │ free       │  │- Isolation           │ │ │
    │  │  └────────────┘  └────────────┘  └──────────────────────┘ │ │
    │  └────────────────────────────────────────────────────────────┘ │
    │                              │                                   │
    │                              ▼                                   │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │                    Observability Layer                       │ │
    │  │  ┌────────────┐  ┌────────────┐  ┌──────────────────────┐ │ │
    │  │  │ OpenTele-  │  │ Prometheus │  │ Structured Logging    │ │ │
    │  │  │ metry      │  │ Metrics    │  │ (JSON format)         │ │ │
    │  │  │ Exporter   │  │            │  │                      │ │ │
    │  │  │            │  │- /metrics  │  │- Structured fields    │ │ │
    │  │  │- OTLP      │  │- Histograms│  │- Log levels          │ │ │
    │  │  │- GCM       │  │- Counters  │  │- Correlation IDs     │ │ │
    │  │  │- Trace     │  │- Gauges    │  │- Timestamps          │ │ │
    │  │  └────────────┘  └────────────┘  └──────────────────────┘ │ │
    │  └────────────────────────────────────────────────────────────┘ │
    │                                                                   │
    │  Ports: 8080 (HTTP), 9090 (Metrics), 9100 (Distribution)        │
    └─────────────────────────────────────────────────────────────────┘
```

---

## 5. Network Architecture

```
    ┌─────────────────────────────────────────────────────────────────┐
    │                    VPC: erlmcp-vpc                               │
    │    Routing Mode: REGIONAL                                       │
    │    MTU: 1460 (Cloud VPN/Interconnect compatible)                │
    │                                                                   │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │  Subnet: erlmcp-subnet-us-central1                          │ │
    │  │  CIDR: 10.0.0.0/24                                          │ │
    │  │  Region: us-central1                                        │ │
    │  │  Private IP Google Access: ENABLED                          │ │
    │  │                                                           │ │
    │  │  Secondary Ranges:                                         │ │
    │  │    - Pods: 10.1.0.0/16                                    │ │
    │  │    - Services: 10.2.0.0/16                                │ │
    │  │                                                           │ │
    │  │  Flow Logs: ENABLED (5s interval, 50% sampling)          │ │
    │  └────────────────────────────────────────────────────────────┘ │
    │                                                                   │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │  Cloud Router + Cloud NAT                                  │ │
    │  │  ASN: 65001                                                 │ │
    │  │  NAT: AUTO_ONLY (cost-optimized)                           │ │
    │  │  Logging: ENABLED (ALL filter)                             │ │
    │  │  Ports: 64-65536 per VM                                    │ │
    │  └────────────────────────────────────────────────────────────┘ │
    │                                                                   │
    │  Firewall Rules (Priority Order):                              │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │  1000: Internal (10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)│ │
    │  │        ALLOW TCP 0-65535, UDP 0-65535, ICMP               │ │
    │  │                                                           │ │
    │  │  1001: SSH (0.0.0.0/0) [RISK]                              │ │
    │  │        ALLOW TCP 22                                        │ │
    │  │                                                           │ │
    │  │  1002: Health Checks (Google IPs)                          │ │
    │  │        ALLOW TCP 8080, 9090                                │ │
    │  │                                                           │ │
    │  │  1003: HTTPS Public (0.0.0.0/0) [Optional]                 │ │
    │  │        ALLOW TCP 443                                       │ │
    │  │                                                           │ │
    │  │  1004: GKE Master Access (10.0.0.0/8)                      │ │
    │  │        ALLOW TCP 10250                                     │ │
    │  │                                                           │ │
    │  │  65534: Deny All (0.0.0.0/0) [Optional]                    │ │
    │  │        DENY TCP 0-65535, UDP 0-65535                      │ │
    │  └────────────────────────────────────────────────────────────┘ │
    └─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                     Internet (via Cloud NAT)                    │
    │                                                                   │
    │  Egress: Private nodes -> Cloud NAT -> Internet                 │
    │  Ingress: Cloud LB -> Firewall -> Nodes                         │
    └─────────────────────────────────────────────────────────────────┘
```

---

## 6. Data Flow Diagram

```
    ┌─────────────┐
    │   Client    │
    │ (MCP Client)│
    └──────┬──────┘
           │ 1. Connect (TCP/HTTP/WebSocket)
           │
           ▼
    ┌───────────────────────────────────────────────────────────────┐
    │                      Load Balancer                             │
    │  - Global HTTPS LB (GKE/GCE)                                   │
    │  - Cloud Run ingress                                           │
    │  - SSL/TLS termination                                         │
    └──────┬────────────────────────────────────────────────────────┘
           │ 2. Route to healthy instance
           │
           ▼
    ┌───────────────────────────────────────────────────────────────┐
    │                    erlmcp Instance                             │
    │                                                                   │
    │  ┌─────────────────────────────────────────────────────────┐   │
    │  │ 3. Receive Request (Transport Layer)                     │   │
    │  │    - Validate TLS certificate                            │   │
    │  │    - Check rate limit (per client)                       │   │
    │  │    - Establish session context                           │   │
    │  └─────────────────────────────────────────────────────────┘   │
    │                           │                                     │
    │                           ▼                                     │
    │  ┌─────────────────────────────────────────────────────────┐   │
    │  │ 4. Parse JSON-RPC (Handler Layer)                        │   │
    │  │    - Validate JSON schema                                │   │
    │  │    - Extract method and params                           │   │
    │  │    - Check authorization (JWT/API key)                   │   │
    │  └─────────────────────────────────────────────────────────┘   │
    │                           │                                     │
    │                           ▼                                     │
    │  ┌─────────────────────────────────────────────────────────┐   │
    │  │ 5. Execute Tool Call (Application Layer)                 │   │
    │  │    - Spawn Erlang process (isolated)                    │   │
    │  │    - Execute tool logic                                 │   │
    │  │    - Collect result                                     │   │
    │  │    - Terminate process                                   │   │
    │  └─────────────────────────────────────────────────────────┘   │
    │                           │                                     │
    │                           ▼                                     │
    │  ┌─────────────────────────────────────────────────────────┐   │
    │  │ 6. Emit Observability (OTEL Layer)                       │   │
    │  │    - Counter: requests_total, errors_total              │   │
    │  │    - Histogram: request_duration_seconds                │   │
    │  │    - Log: structured JSON to Cloud Logging               │   │
    │  │    - Span: distributed trace to Cloud Trace              │   │
    │  └─────────────────────────────────────────────────────────┘   │
    │                           │                                     │
    │                           ▼                                     │
    │  ┌─────────────────────────────────────────────────────────┐   │
    │  │ 7. Format Response (Transport Layer)                     │   │
    │  │    - Wrap in JSON-RPC response format                   │   │
    │  │    - Add correlation ID                                  │   │
    │  │    - Serialize to JSON                                   │   │
    │  └─────────────────────────────────────────────────────────┘   │
    │                           │                                     │
    └───────────────────────────┼─────────────────────────────────────┘
                               │ 8. Send Response
                               ▼
                        ┌─────────────┐
                        │   Client    │
                        │ (Result/Error)│
                        └─────────────┘
```

---

## 7. Observability Integration

```
    ┌─────────────────────────────────────────────────────────────────┐
    │                    erlmcp Application                           │
    │  ┌─────────┐  ┌─────────┐  ┌─────────┐                         │
    │  │ erlmcp  │  │ erlmcp  │  │ erlmcp  │                         │
    │  │ Pod 1   │  │ Pod 2   │  │ Pod 3   │                         │
    │  └────┬────┘  └────┬────┘  └────┬────┘                         │
    │       │            │            │                              │
    │       └────────────┴────────────┘                              │
    │                    │                                           │
    │  ┌─────────────────┴─────────────────────────────────────────┐ │
    │  │          OpenTelemetry Collector (sidecar/in-process)      │ │
    │  │  - Metrics: Prometheus format (scrape port 9090)           │ │
    │  │  - Logs: JSON structured (stdout)                         │ │
    │  │  - Traces: OTLP over gRPC                                 │ │
    │  └────────────────────────┬──────────────────────────────────┘ │
    └───────────────────────────┼─────────────────────────────────────┘
                               │
                               ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                   Google Cloud Operations                       │
    │  ┌───────────────────────────────────────────────────────────┐ │
    │  │                    Ingestion Pipeline                      │ │
    │  │  - Logging: Cloud Logging agents                         │ │
    │  │  - Monitoring: Cloud Monitoring API                       │ │
    │  │  - Tracing: Cloud Trace                                   │ │
    │  └───────────────────────────────────────────────────────────┘ │
    │                              │                                  │
    │  ┌───────────────────────────────┬────────────────────────────┐ │
    │  │  Cloud Logging                 │  Cloud Monitoring           │ │
    │  │  - Log Router                   │  - Metric Service           │ │
    │  │  - Log Sinks (BQ, GCS)          │  - Alert Policies (6)      │ │
    │  │  - Log Exclusions              │  - Dashboards (4)          │ │
    │  └───────────────────────────────┴────────────────────────────┘ │
    │                              │                                  │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │              Alert Evaluation & Notification                │ │
    │  │  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────────┐   │ │
    │  │  │  Error  │  │ Latency │  │ Memory  │  │ Health      │   │ │
    │  │  │  Rate   │  │  SLO    │  │  Usage  │  │ Check       │   │ │
    │  │  └────┬────┘  └────┬────┘  └────┬────┘  └──────┬──────┘   │ │
    │  │       └─────────────┼─────────────┘                 │        │ │
    │  │                     ▼                                  │        │ │
    │  │  ┌────────────────────────────────────────────────────┐    │ │
    │  │  │         Notification Channels (4)                  │    │ │
    │  │  │  Email | PagerDuty | Slack | Webhook              │    │ │
    │  │  └────────────────────────────────────────────────────┘    │ │
    │  └────────────────────────────────────────────────────────────┘ │
    └─────────────────────────────────────────────────────────────────┘
```

**Custom Metrics Exported:**
1. `custom.googleapis.com/erlmcp/http/latency` - Request latency (seconds)
2. `custom.googleapis.com/erlmcp/http/requests` - Request count
3. `custom.googleapis.com/erlmcp/connections/active` - Active connections
4. `custom.googleapis.com/erlmcp/erlang/processes` - Process count
5. `custom.googleapis.com/erlmcp/memory/usage` - Memory usage (bytes)

---

## 8. Secret Management Architecture

```
    ┌─────────────────────────────────────────────────────────────────┐
    │                   Google Secret Manager                          │
    │                                                                   │
    │  Secrets (11 total):                                             │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │ 1. erlmcp-erlang-cookie  (Cluster security)                │ │
    │  │ 2. erlmcp-db-password    (Database auth)                  │ │
    │  │ 3. erlmcp-redis-password (Redis auth)                     │ │
    │  │ 4. erlmcp-tls-cert       (TLS certificate)                 │ │
    │  │ 5. erlmcp-tls-key        (TLS private key)                │ │
    │  │ 6. erlmcp-ca-bundle      (CA certificates)                │ │
    │  │ 7. erlmcp-jwt-private-key (JWT signing)                   │ │
    │  │ 8. erlmcp-jwt-public-key  (JWT verification)              │ │
    │  │ 9. erlmcp-grafana-password (Grafana auth)                │ │
    │  │ 10. erlmcp-backup-key     (Backup encryption)             │ │
    │  │ 11. erlmcp-otel-ca-cert   (OTEL mTLS)                    │ │
    │  └────────────────────────────────────────────────────────────┘ │
    │                                                                   │
    │  Replication: AUTOMATIC (multi-region)                          │
    │  Rotation: 90 days (automatic)                                   │
    └─────────────────────────────────────────────────────────────────┘
                              │
                              │ IAM: roles/secretmanager.secretAccessor
                              ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                   Workload Identity                              │
    │                                                                   │
    │  Kubernetes Service Account (KSA)                                │
    │       │                                                           │
    │       │ iam.gke.io/gcp-service-account annotation                │
    │       ▼                                                           │
    │  Google Service Account (GSA)                                     │
    │       │                                                           │
    │       │ IAM binding: roles/iam.workloadIdentityUser              │
    │       ▼                                                           │
    │  Secret Manager API Access                                        │
    └─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
    ┌─────────────────────────────────────────────────────────────────┐
    │                    erlmcp Container/Pod                           │
    │                                                                   │
    │  Secret Access Patterns:                                          │
    │  ┌────────────────────────────────────────────────────────────┐ │
    │  │ GKE:   Environment variable via K8s Secret                   │ │
    │  │        (synced from Secret Manager)                         │ │
    │  │                                                               │ │
    │  │ Cloud Run: Direct Secret Manager reference                   │ │
    │  │            (secretKeyRef in env)                            │ │
    │  │                                                               │ │
    │  │ Compute Engine: Startup script pulls from Secret Manager     │ │
    │  │                  (uses GSA authentication)                  │ │
    │  └────────────────────────────────────────────────────────────┘ │
    └─────────────────────────────────────────────────────────────────┘
```

---

## 9. Multi-Region Architecture (Future Enhancement)

```
    ┌─────────────────────────────────────────────────────────────────────┐
    │                         Multi-Region Deployment                      │
    │                                                                      │
    │  ┌──────────────────────┐  ┌──────────────────────┐               │
    │  │   Region: us-central1  │  │   Region: us-east1   │               │
    │  │  ┌──────────────────┐  │  │  ┌──────────────────┐ │              │
    │  │  │   GKE Cluster     │  │  │  │   GKE Cluster     │ │              │
    │  │  │   (Primary)       │  │  │  │   (Standby)       │ │              │
    │  │  │   3 zones         │  │  │  │   3 zones         │ │              │
    │  │  │   9 nodes total    │  │  │  │   9 nodes total    │ │              │
    │  │  └─────────┬─────────┘  │  │  └─────────┬─────────┘ │              │
    │  │            │            │  │            │           │              │
    │  └────────────┼────────────┘  └────────────┼───────────┘              │
    │               │                            │                          │
    └───────────────┼────────────────────────────┼──────────────────────────┘
                    │                            │
                    │          ┌─────────────────┴─────────────────┐        │
                    │          │       Global Load Balancer         │        │
                    │          │   (Cloud Load Balancing)          │        │
                    │          │   - Traffic steering              │        │
                    │          │   - Failover detection           │        │
                    │          └─────────────────┬─────────────────┘        │
                    │                            │                         │
                    │  Primary Route             │ Standby Route            │
                    │  (99% traffic)             │ (1% traffic / DR)        │
                    ▼                            ▼                         │
    ┌─────────────────────────────────────────────────────────────────────┤
    │                    Data Replication Layer                          │
    │  ┌────────────────────┐  ┌────────────────────┐  ┌───────────────┐ │
    │  │   Cloud Spanner     │  │   Cloud SQL         │  │ Cloud Storage │ │
    │  │   (Multi-region)    │  │   (Primary-Standby) │  │ (Multi-region)│ │
    │  │                     │  │                     │  │               │ │
    │  │  - Async config     │  │  - Synchronous      │  │ - Async       │ │
    │  │  - Strong reads     │  │  - Automatic failover│  │ - Replication │ │
    │  └────────────────────┘  └────────────────────┘  └───────────────┘ │
    └─────────────────────────────────────────────────────────────────────┘
```

**Target SLA: 99.99% (4.32 minutes/month downtime)**
