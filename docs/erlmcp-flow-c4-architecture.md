# erlmcp-flow C4 Architecture Diagrams

---

## C1: System Context

```mermaid
graph TB
    User["👤 User/Orchestrator<br/>(API Client)"]
    Flow["🧠 erlmcp-flow<br/>(Agent Orchestration System)"]
    LLM["🤖 LLM Backend<br/>(Claude/GPT)"]
    External["🌐 External Systems<br/>(APIs, Databases)"]

    User -->|spawn_agent<br/>assign_task<br/>query_status| Flow
    Flow -->|prompt<br/>completion| LLM
    Flow -->|API calls<br/>data access| External
    Flow -->|metrics<br/>traces<br/>logs| Monitoring["📊 Monitoring<br/>(OTEL, Prometheus)"]

    style Flow fill:#4A90E2,color:#fff
    style User fill:#50C878,color:#fff
    style LLM fill:#FF6B6B,color:#fff
    style External fill:#9B59B6,color:#fff
    style Monitoring fill:#F39C12,color:#fff
```

---

## C2: Container Architecture

```mermaid
graph TB
    subgraph Client["Client Layer"]
        API["API Gateway<br/>(REST/gRPC)"]
        Orchestrator["Task Orchestrator<br/>(User Code)"]
    end

    subgraph erlmcp_flow["erlmcp-flow System"]
        subgraph core["🏗️  erlmcp_core<br/>(Agent Foundation + Routing)"]
            AgentSpawner["Agent Spawner<br/>(gen_server)"]
            AgentReg["Agent Registry<br/>(gproc)"]
            AgentState["Agent State Mgr<br/>(gen_server)"]
            AgentHealth["Agent Health Monitor<br/>(gen_server)"]
            Router["Routing Engine<br/>(gen_server)"]
        end

        subgraph transports["🔌 erlmcp_transports<br/>(Coordination)"]
            SwarmMgr["Swarm Manager<br/>(gen_server)"]
            TopoMgr["Topology Manager<br/>(gen_server)"]
            ConsensusMgr["Consensus Manager<br/>(gen_server)"]
            MsgRouter["Message Router<br/>(gen_server)"]
        end

        subgraph observability["🧠 erlmcp_observability<br/>(Intelligence)"]
            LearningSystem["Learning System<br/>(gen_server)"]
            TrajectoryRec["Trajectory Recorder<br/>(gen_server)"]
            MemorySubsystem["Memory Subsystem<br/>(gen_server)"]
            SemanticStore["Semantic Store<br/>(HNSW + sql.js)"]
        end

        subgraph validation["🔒 erlmcp_validation<br/>(Security)"]
            AIDefence["AIDefence Engine<br/>(gen_server)"]
            AuditMgr["Audit Manager<br/>(gen_server)"]
            AuditLog["Audit Log<br/>(Immutable)"]
            ComplianceValidator["Compliance Validator<br/>(gen_server)"]
        end
    end

    subgraph Storage["Storage Layer"]
        AgentStateDB["Agent State<br/>(ETS/DETS)"]
        AuditLogDB["Audit Log<br/>(S3/Persistent)"]
        PatternDB["Pattern Bank<br/>(Vector Store)"]
    end

    API -->|spawn_agent| AgentSpawner
    Orchestrator -->|route_task<br/>assign_work| Router

    AgentSpawner -->|register| AgentReg
    AgentSpawner -->|init state| AgentState
    AgentState -->|read/write| AgentStateDB

    Router -->|routing_decision| LearningSystem
    LearningSystem -->|trajectory| TrajectoryRec

    AgentSpawner -->|health_check| AgentHealth

    Router -->|route via topology| SwarmMgr
    SwarmMgr -->|coordinate| TopoMgr
    TopoMgr -->|send message| MsgRouter

    MsgRouter -->|agent input| AIDefence
    AIDefence -->|threat verdict| MsgRouter

    AIDefence -->|audit| AuditMgr
    AuditMgr -->|append| AuditLog
    AuditLog -->|persist| AuditLogDB

    MemorySubsystem -->|store/search| SemanticStore
    SemanticStore -->|persist| PatternDB

    TrajectoryRec -->|pattern| MemorySubsystem

    MsgRouter -->|consensus| ConsensusMgr
    ConsensusMgr -->|vote| MsgRouter

    style core fill:#4A90E2,color:#fff
    style transports fill:#7B68EE,color:#fff
    style observability fill:#50C878,color:#fff
    style validation fill:#FF6B6B,color:#fff
    style Storage fill:#F39C12,color:#fff
    style Client fill:#95A5A6,color:#fff
```

---

## C3: Agent Foundation (Core App)

```mermaid
graph TB
    subgraph Input["Input API"]
        SpawnAPI["spawn_agent(config)"]
        QueryAPI["get_agent(id)"]
        AssignAPI["assign_task(agent_id, task)"]
    end

    subgraph Spawning["Spawning Pipeline"]
        Validate["1️⃣  Validate Config<br/>(check_fields)"]
        CreateChild["2️⃣  Create Child<br/>(supervisor:start_child)"]
        Register["3️⃣  Register<br/>(gproc)"]
        InitState["4️⃣  Init State<br/>(gen_server:call)"]
        Return["5️⃣  Return Agent<br/>{ok, agent()}"]
    end

    subgraph StateManagement["State Backend"]
        ETS["ETS Cache<br/>(O(1) read)"]
        DETS["DETS Durable<br/>(crash recovery)"]
        Mnesia["Mnesia Distributed<br/>(cluster)"]
    end

    subgraph Monitoring["Health & Monitoring"]
        Supervisor["Supervisor<br/>(simple_one_for_one)"]
        HealthMonitor["Health Monitor<br/>(periodic checks)"]
        Restart["Restart Logic<br/>(max 5 in 60s)"]
    end

    subgraph Registry["Global Registry"]
        GProc["gproc Registry<br/>(O(log N) lookup)"]
        QueryFuncs["Query Functions<br/>(list_agents, find_agent)"]
    end

    SpawnAPI --> Validate
    Validate --> CreateChild
    CreateChild --> Register
    Register --> InitState
    InitState --> Return

    Return --> StateManagement
    InitState --> ETS

    Supervisor -.monitor.-> HealthMonitor
    HealthMonitor -.check.-> Restart
    Restart -.restart.-> CreateChild

    Register --> GProc
    QueryAPI --> GProc
    GProc --> QueryFuncs

    AssignAPI -->|check load| HealthMonitor
    AssignAPI -->|send task| Supervisor

    style Spawning fill:#4A90E2,color:#fff
    style StateManagement fill:#50C878,color:#fff
    style Monitoring fill:#FF6B6B,color:#fff
    style Registry fill:#F39C12,color:#fff
    style Input fill:#95A5A6,color:#fff
```

---

## C4: Coordination (Transports App)

```mermaid
graph TB
    subgraph Input["Input API"]
        CreateSwarm["create_swarm<br/>(agents, topology)"]
        CreateTopo["create_topology<br/>(type, config)"]
        SendMsg["send_message<br/>(from, to, msg)"]
        Broadcast["broadcast_message<br/>(topology, msg)"]
    end

    subgraph Topologies["Topology Types"]
        Mesh["MESH<br/>(fully connected)"]
        Hierarchical["HIERARCHICAL<br/>(leader-follower)"]
        Ring["RING<br/>(circular)"]
        Star["STAR<br/>(hub-spoke)"]
    end

    subgraph Management["Topology Management"]
        TopoMgr["Topology Manager<br/>(gen_server)"]
        AddNode["add_node_to_topology"]
        RemoveNode["remove_node_from_topology"]
        GetTopo["get_topology"]
    end

    subgraph Messaging["Message Routing"]
        MsgRouter["Message Router<br/>(gen_server)"]
        EdgeQueue["Edge Queue<br/>(FIFO per edge)"]
        Retry["Retry Logic<br/>(exponential backoff)"]
    end

    subgraph Consensus["Consensus Protocol"]
        ConsensusMgr["Consensus Manager<br/>(gen_server)"]
        Propose["propose_consensus"]
        Vote["vote_consensus"]
        LeaderElect["Leader Election<br/>(Raft-based)"]
    end

    subgraph Redundancy["Fault Tolerance"]
        Redundancy_lvl["Redundancy Level<br/>(1-3 paths)"]
        FailoverPath["Failover Path<br/>(alternate edges)"]
        HeartbeatDetect["Heartbeat Detection<br/>(5s interval)"]
    end

    CreateSwarm --> CreateTopo
    CreateTopo --> Management

    Management -->|manager for type| Topologies

    SendMsg --> MsgRouter
    Broadcast --> MsgRouter

    MsgRouter -->|route via topology| EdgeQueue
    EdgeQueue -->|FIFO delivery| Messaging

    EdgeQueue -.ack.-> Retry
    Retry -.failed.-> FailoverPath
    FailoverPath -->|alternate edge| EdgeQueue

    Broadcast -->|multi-recipient| ConsensusMgr
    ConsensusMgr --> Propose
    Propose --> Vote
    Vote --> LeaderElect

    HeartbeatDetect -.detect failure.-> RemoveNode
    RemoveNode -->|update topology| TopoMgr

    TopoMgr -->|supports redundancy| Redundancy_lvl

    style Topologies fill:#7B68EE,color:#fff
    style Management fill:#4A90E2,color:#fff
    style Messaging fill:#50C878,color:#fff
    style Consensus fill:#FF6B6B,color:#fff
    style Redundancy fill:#F39C12,color:#fff
    style Input fill:#95A5A6,color:#fff
```

---

## C4: Intelligence (Observability App)

```mermaid
graph TB
    subgraph Input["Input API"]
        RouteTask["route_task<br/>(context)"]
        StartTraj["start_trajectory<br/>(agent, task)"]
        RecordStep["record_step<br/>(trajectory, step)"]
        SearchPatterns["search_patterns<br/>(query)"]
    end

    subgraph Routing["Routing Decision"]
        Router["Routing Engine<br/>(gen_server)"]
        MetricsLoad["Load Recent Metrics<br/>(ETS cache, ≤5min)"]
        CapCheck["Capability Check<br/>(agent caps vs task)"]
        PatternMatch["HNSW Pattern Match<br/>(similar tasks)"]
        ModelInfer["Model Inference<br/>(learned policy)"]
        RankAgents["Rank Agents<br/>(confidence)"]
    end

    subgraph Learning["Learning System"]
        LearningMgr["Learning System<br/>(gen_server)"]
        TrajRecorder["Trajectory Recorder<br/>(gen_server)"]
        TrajStorage["Trajectory Storage<br/>(ETS)"]
        TriggerCycle["Learning Cycle<br/>(≥10 trajectories)"]
    end

    subgraph Consolidation["Consolidation Strategies"]
        SONA["SONA<br/>(Self-Organizing)"]
        EWC["EWC++<br/>(Elastic Weight)"]
        PreserveCritical["Preserve Critical<br/>(important patterns)"]
    end

    subgraph Memory["Semantic Memory"]
        MemoryMgr["Memory Subsystem<br/>(gen_server)"]
        SemanticStore["Semantic Store<br/>(sql.js + HNSW)"]
        PatternBank["Pattern Bank<br/>(vector embeddings)"]
        ONNX["ONNX Embedder<br/>(all-MiniLM-L6-v2)"]
        MemoryReaper["Memory Reaper<br/>(TTL management)"]
    end

    subgraph Metrics["Metrics Collection"]
        MetricsCollector["Metrics Collector<br/>(gen_server)"]
        Histogram["Histogram<br/>(latencies)"]
        Percentile["Percentile<br/>(p50, p95, p99)"]
    end

    RouteTask --> Router
    Router --> MetricsLoad
    MetricsLoad --> CapCheck
    CapCheck --> PatternMatch
    PatternMatch --> ModelInfer
    ModelInfer --> RankAgents
    RankAgents -->|return routing_decision| Input

    StartTraj --> TrajRecorder
    RecordStep --> TrajRecorder
    TrajRecorder -->|store| TrajStorage
    TrajStorage -.accumulate.-> TriggerCycle
    TriggerCycle --> Learning

    TriggerCycle --> Consolidation
    SONA -.consolidate.-> EWC
    EWC -.preserve.-> PreserveCritical

    TrajRecorder -->|extract pattern| MemoryMgr
    MemoryMgr -->|store| PatternBank

    PatternMatch -.search.-> SemanticStore
    SearchPatterns --> SemanticStore

    ONNX -->|embed text| SemanticStore
    SemanticStore -->|HNSW index| PatternBank

    MemoryReaper -.cleanup old.-> PatternBank

    MetricsLoad -.from.-> MetricsCollector
    RankAgents -->|record decision| MetricsCollector
    MetricsCollector -->|compute| Histogram
    Histogram -->|aggregate| Percentile

    style Routing fill:#4A90E2,color:#fff
    style Learning fill:#50C878,color:#fff
    style Consolidation fill:#7B68EE,color:#fff
    style Memory fill:#FF6B6B,color:#fff
    style Metrics fill:#F39C12,color:#fff
    style Input fill:#95A5A6,color:#fff
```

---

## C4: Security (Validation App)

```mermaid
graph TB
    subgraph Input["Input API"]
        ScanInput["scan_input<br/>(text, context)"]
        Analyze["analyze_threat<br/>(assessment)"]
        LogEvent["log_audit_event<br/>(event)"]
        CheckCompliance["check_compliance<br/>(agent, action)"]
    end

    subgraph Threats["Threat Detection"]
        AIDefence["AIDefence Engine<br/>(gen_server)"]
        ThreatDetector["Threat Detector<br/>(Soufflé rules)"]
        PatternMatcher["Pattern Matcher<br/>(similarity search)"]
        PIIScanner["PII Scanner<br/>(regex + ML)"]
        Verdict["Threat Verdict<br/>(allow|block|warn)"]
    end

    subgraph ThreatTypes["Threat Categories"]
        PromptInjection["Prompt Injection<br/>(syntax tricks)"]
        Jailbreak["Jailbreak<br/>(bypass rules)"]
        PIIExposure["PII Exposure<br/>(sensitive data)"]
        Poisoning["Model Poisoning<br/>(malicious data)"]
        Escalation["Privilege Escalation<br/>(policy bypass)"]
    end

    subgraph Audit["Audit Trail"]
        AuditMgr["Audit Manager<br/>(gen_server)"]
        AuditLogger["Audit Logger<br/>(append-only)"]
        ChainOfCustody["Chain of Custody<br/>(hash-chained)"]
        AuditIndexer["Audit Indexer<br/>(full-text search)"]
    end

    subgraph Compliance["Compliance"]
        ComplianceValidator["Compliance Validator<br/>(gen_server)"]
        PolicyEngine["Policy Engine<br/>(rule interpreter)"]
        ComplianceReporter["Compliance Reporter<br/>(generator)"]
        PolicyCache["Policy Cache<br/>(TTL)"]
    end

    subgraph Secrets["Secrets & Encryption"]
        SecretMgr["Secret Manager<br/>(gen_server)"]
        EncryptService["Encryption Service<br/>(AES-256)"]
        KeyRotation["Key Rotation<br/>(≤90 days)"]
    end

    ScanInput --> AIDefence
    AIDefence --> ThreatDetector
    ThreatDetector -->|match patterns| ThreatTypes
    ThreatDetector --> PatternMatcher

    AIDefence --> PIIScanner
    PIIScanner -->|detect categories| ThreatTypes

    PatternMatcher -->|compute assessment| Threats
    Verdict -->|return to caller| Input

    Verdict -->|always log| AuditMgr
    LogEvent --> AuditMgr
    CheckCompliance --> AuditMgr

    AuditMgr --> AuditLogger
    AuditLogger -->|hash & chain| ChainOfCustody
    AuditLogger --> AuditIndexer

    CheckCompliance --> ComplianceValidator
    ComplianceValidator -->|check rules| PolicyEngine
    PolicyEngine -.cache.-> PolicyCache
    PolicyEngine -->|report| ComplianceReporter

    Verdict -.if threat.-> SecretMgr
    SecretMgr -.encrypt logs.-> EncryptService

    KeyRotation -.rotate keys.-> EncryptService
    EncryptService -->|encrypt| AuditLogger

    Analyze -->|deep analysis| PatternMatcher

    style Threats fill:#FF6B6B,color:#fff
    style ThreatTypes fill:#E74C3C,color:#fff
    style Audit fill:#F39C12,color:#fff
    style Compliance fill:#4A90E2,color:#fff
    style Secrets fill:#9B59B6,color:#fff
    style Input fill:#95A5A6,color:#fff
```

---

## Subsystem Integration Diagram

```mermaid
graph LR
    subgraph Agent["Agent Foundation<br/>(erlmcp_core)"]
        SpawnAgent["spawn_agent()"]
        AgentRegistry["Registry"]
    end

    subgraph Coord["Coordination<br/>(erlmcp_transports)"]
        SwarmMgr["Swarm Manager"]
        TopoMgr["Topology Manager"]
    end

    subgraph Intel["Intelligence<br/>(erlmcp_observability)"]
        Router["Routing Engine"]
        Learning["Learning System"]
        Memory["Memory Store"]
    end

    subgraph Security["Security<br/>(erlmcp_validation)"]
        AIDefence["AIDefence"]
        Audit["Audit Manager"]
    end

    SpawnAgent -->|"agent_spawned<br/>{id, role, caps}"| SwarmMgr
    SwarmMgr -->|"assign_to_swarm<br/>{topology}"| Agent

    SpawnAgent -->|"agent_created<br/>{metrics}"| Router

    Router -->|"routing_decision<br/>{confidence}"| Agent
    Agent -->|"task_completed<br/>{latency, result}"| Learning

    Learning -->|"trajectory_recorded<br/>{pattern}"| Memory
    Memory -->|"pattern_matched<br/>{similarity}"| Router

    SwarmMgr -->|"send_message<br/>{from, to}"| TopoMgr
    TopoMgr -->|"route_via_topology"| AIDefence

    AIDefence -->|"threat_verdict<br/>{verdict}"| TopoMgr
    AIDefence -->|"scan_result<br/>{threat}"| Audit
    Audit -->|"audit_event<br/>{immutable}"| Security

    style Agent fill:#4A90E2,color:#fff
    style Coord fill:#7B68EE,color:#fff
    style Intel fill:#50C878,color:#fff
    style Security fill:#FF6B6B,color:#fff
```

---

## Failure Recovery Workflows

### F-1: Agent Crash Recovery

```mermaid
graph TD
    Crash["Agent Process Crashes"]
    Detect["Supervisor Detects<br/>(Down signal)"]
    Increment["Increment<br/>Restart Counter"]
    Check["Counter < 5<br/>in 60s?"]
    Restart["Restart Agent<br/>(regenerate PID)"]
    Recover["Restore State<br/>(ETS backend)"]
    Notify["Notify Swarm<br/>(topology update)"]
    GiveUp["Give Up<br/>(too many restarts)"]
    Redistribute["Redistribute<br/>Tasks to Others"]

    Crash --> Detect
    Detect --> Increment
    Increment --> Check
    Check -->|YES| Restart
    Check -->|NO| GiveUp
    Restart --> Recover
    Recover --> Notify
    GiveUp --> Redistribute

    style Crash fill:#FF6B6B,color:#fff
    style Detect fill:#F39C12,color:#fff
    style Restart fill:#50C878,color:#fff
    style Recover fill:#4A90E2,color:#fff
    style GiveUp fill:#E74C3C,color:#fff
```

### F-5: Message Loss Recovery

```mermaid
graph TD
    Send["Send Message<br/>on Edge"]
    Wait["Wait for ACK<br/>(timeout=1s)"]
    Timeout["Timeout<br/>Triggered"]
    Retry["Retry Send<br/>(exponential backoff)"]
    RetryCount["Retry Count<br/>1, 2, 3"]
    AllRetries["All Retries<br/>Failed?"]
    Reroute["Reroute via<br/>Alternate Path"]
    Report["Report to Leader<br/>(message loss)"]

    Send --> Wait
    Wait -->|timeout| Timeout
    Timeout --> Retry
    Retry --> Wait
    Wait -->|ack received| Report
    Retry --> RetryCount
    RetryCount -->|count < 3| Retry
    RetryCount -->|count >= 3| Reroute
    Reroute -->|no paths| Report

    style Send fill:#4A90E2,color:#fff
    style Timeout fill:#FF6B6B,color:#fff
    style Retry fill:#F39C12,color:#fff
    style Reroute fill:#50C878,color:#fff
    style Report fill:#9B59B6,color:#fff
```

---

## Technology Stack

```
┌─────────────────────────────────────────────────────┐
│ Application Layer                                   │
│  erlmcp-flow (4 OTP Apps × 4 Subsystems)           │
└────────────────────────┬────────────────────────────┘
                         │
┌────────────────────────┴────────────────────────────┐
│ OTP Framework (Erlang 28.3.1)                       │
│ ├─ gen_server, gen_statem, supervisor              │
│ ├─ gproc (process registry)                        │
│ └─ OTEL (observability)                            │
└────────────────────────┬────────────────────────────┘
                         │
┌────────────────────────┴────────────────────────────┐
│ Persistent Storage                                  │
│ ├─ ETS (agent state cache, O(1))                   │
│ ├─ DETS (durable sessions)                         │
│ ├─ Mnesia (distributed state, optional)            │
│ ├─ S3 (audit logs, long-term)                      │
│ └─ sql.js (semantic store, HNSW)                   │
└────────────────────────┬────────────────────────────┘
                         │
┌────────────────────────┴────────────────────────────┐
│ External Dependencies                               │
│ ├─ ONNX Runtime (all-MiniLM-L6-v2 embeddings)      │
│ ├─ Soufflé (threat detection rules)                │
│ ├─ OpenTelemetry (metrics/traces)                  │
│ ├─ Prometheus (scraping metrics)                   │
│ └─ TLS/mTLS (transport encryption)                 │
└─────────────────────────────────────────────────────┘
```

---

## Deployment Architecture

```mermaid
graph TB
    subgraph Kubernetes["Kubernetes Cluster"]
        subgraph StatefulSet["erlmcp-flow StatefulSet"]
            Pod1["Pod-0 (Primary)<br/>Port 4369"]
            Pod2["Pod-1 (Replica)<br/>Port 4369"]
            Pod3["Pod-2 (Replica)<br/>Port 4369"]
        end

        subgraph Services["Services"]
            Headless["erlmcp-flow<br/>(headless DNS)"]
            External["erlmcp-flow-api<br/>(LoadBalancer)"]
            Audit["erlmcp-flow-audit<br/>(write-only)"]
        end

        subgraph Storage["PersistentVolumes"]
            PVC_State["PVC: agent_state<br/>(EBS gp3)"]
            PVC_Audit["PVC: audit_log<br/>(S3)"]
            PVC_Memory["PVC: memory_store<br/>(SSD)"]
        end
    end

    Pod1 -.cluster.-> Pod2
    Pod2 -.cluster.-> Pod3

    Headless -->|service discovery| Pod1
    Headless -->|service discovery| Pod2
    Headless -->|service discovery| Pod3

    External -->|lb:8080| Pod1
    External -->|lb:8080| Pod2
    External -->|lb:8080| Pod3

    Pod1 -->|mount| PVC_State
    Pod2 -->|mount| PVC_State
    Pod3 -->|mount| PVC_State

    Pod1 -->|append-only| PVC_Audit
    Pod2 -->|append-only| PVC_Audit
    Pod3 -->|append-only| PVC_Audit

    Pod1 -->|local cache| PVC_Memory
    Pod2 -->|local cache| PVC_Memory
    Pod3 -->|local cache| PVC_Memory

    Audit -->|metrics endpoint| Prometheus["Prometheus<br/>:9090"]
    Headless -->|OTEL exporter| Jaeger["Jaeger Tracing<br/>:14250"]

    style Kubernetes fill:#4A90E2,color:#fff
    style StatefulSet fill:#7B68EE,color:#fff
    style Services fill:#50C878,color:#fff
    style Storage fill:#FF6B6B,color:#fff
```

---

**Document Version**: v1.0.0
**Last Updated**: 2026-02-01
**Next Review**: 2026-05-01

