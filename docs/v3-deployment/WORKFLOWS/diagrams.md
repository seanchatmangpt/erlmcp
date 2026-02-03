# erlmcp v3 Deployment Workflow Diagrams

**Version:** 3.0.0
**Last Updated:** 2026-02-02

This document contains all workflow diagrams in Mermaid format for easy rendering and modification.

---

## Table of Contents

1. [Event-Driven Architecture](#event-driven-architecture)
2. [Deployment State Machine](#deployment-state-machine)
3. [Multi-Stage Promotion](#multi-stage-promotion)
4. [Approval Gate Flow](#approval-gate-flow)
5. [Rollback Decision Tree](#rollback-decision-tree)
6. [Notification Flow](#notification-flow)
7. [Canary Deployment](#canary-deployment)
8. [Blue-Green Deployment](#blue-green-deployment)
9. [Disaster Recovery](#disaster-recovery)

---

## Event-Driven Architecture

```mermaid
graph TB
    subgraph "Event Sources"
        A[Git Push] --> E[Event Bus]
        B[Pull Request] --> E
        C[Tag Push] --> E
        D[Manual Dispatch] --> E
        F[Scheduled] --> E
        G[Webhook] --> E
    end

    subgraph "Event Processing"
        E --> H[Event Validator]
        H --> I{Valid?}
        I -->|No| J[Reject Event]
        I -->|Yes| K[Route to Workflow]
    end

    subgraph "Workflow Engine"
        K --> L{Event Type?}
        L -->|Push| M[Dev Deployment]
        L -->|PR Merge| N[Staging Deployment]
        L -->|Tag| O[Production Deployment]
        L -->|Manual| P[Custom Deployment]
    end

    subgraph "Deployment Stages"
        M --> Q[Build & Test]
        N --> Q
        O --> Q
        Q --> R[Quality Gates]
        R --> S{All Passed?}
        S -->|No| T[Block & Notify]
        S -->|Yes| U[Deploy]
    end

    subgraph "Post-Deployment"
        U --> V[Health Checks]
        V --> W{Healthy?}
        W -->|No| X[Rollback]
        W -->|Yes| Y[Monitor]
        Y --> Z[Complete]
        X --> Z
    end

    subgraph "Notifications"
        T --> AA[Notify Failed]
        U --> AB[Notify Started]
        X --> AC[Notify Rolled Back]
        Z --> AD[Notify Completed]
    end

    style A fill:#e1f5fe
    style B fill:#e1f5fe
    style C fill:#fff3e0
    style D fill:#e8f5e9
    style E fill:#f3e5f5
    style S fill:#fff9c4
    style W fill:#fff9c4
```

---

## Deployment State Machine

```mermaid
stateDiagram-v2
    [*] --> Pending: Event Triggered
    Pending --> Validating: Pre-flight Checks
    Pending --> Rejected: Checks Failed

    Validating --> Building: Validation Passed
    Validating --> Failed: Validation Failed

    Building --> Testing: Build Success
    Building --> Failed: Build Failed

    Testing --> Approval: Tests Passed
    Testing --> Failed: Tests Failed

    Approval --> Approved: Approved
    Approval --> Rejected: Rejected
    Approval --> Expired: Timeout

    Approved --> Deploying: Start Deployment
    Expired --> Rejected: Auto Reject

    Deploying --> Verifying: Deployment Complete
    Deploying --> RollingBack: Deployment Failed

    Verifying --> Monitoring: Health Checks Passed
    Verifying --> RollingBack: Health Checks Failed

    Monitoring --> Completed: Stable (24h)
    Monitoring --> RollingBack: Degradation Detected

    RollingBack --> Failed: Rollback Complete
    RollingBack --> Monitoring: Recovery Successful

    Completed --> [*]
    Failed --> [*]
    Rejected --> [*]

    note right of Pending: All deployments start here
    note right of Approval: Manual gate for production
    note right of Monitoring: 24h soak period
    note right of RollingBack: Automatic on failure
```

---

## Multi-Stage Promotion

```mermaid
graph LR
    subgraph "Development"
        A[Dev<br/>Auto on Push] --> B[Smoke Tests]
        B --> C{Pass?}
        C -->|No| D[Fix]
        C -->|Yes| E[Dev Deployed]
    end

    subgraph "QA"
        E --> F[PR to Main]
        F --> G[Full Test Suite]
        G --> H{Pass?}
        H -->|No| I[Request Changes]
        H -->|Yes| J[QA Deployed]
    end

    subgraph "Staging"
        J --> K[Merge to Main]
        K --> L[Security & Performance]
        L --> M{Pass?}
        M -->|No| N[Investigate]
        M -->|Yes| O[Staging Deployed]
        O --> P[24h Soak Period]
        P --> Q{Stable?}
        Q -->|No| N
    end

    subgraph "Production"
        Q -->|Yes| R[Tag & Approve]
        R --> S[Change Board Approval]
        S --> T{Approved?}
        T -->|No| U[Reject]
        T -->|Yes| V[Canary 10%]
        V --> W[30m Monitor]
        W --> X{Good?}
        X -->|No| U
        X -->|Yes| Y[Canary 50%]
        Y --> Z[30m Monitor]
        Z --> AA{Good?}
        AA -->|No| U
        AA -->|Yes| AB[Full Rollout]
        AB --> AC[Production Live]
    end

    style A fill:#c8e6c9
    style E fill:#c8e6c9
    style J fill:#fff59d
    style O fill:#fff59d
    style AB fill:#ef9a9a
    style AC fill:#ef5350
```

---

## Approval Gate Flow

```mermaid
graph TB
    subgraph "Gate Request"
        A[Deployment Reaches Gate] --> B{Gate Type?}
        B -->|Automated| C[Execute Check]
        B -->|Manual| D[Request Approval]
    end

    subgraph "Automated Gate"
        C --> E[Run Check]
        E --> F{Result?}
        F -->|Pass| G[Record Pass]
        F -->|Warn| H[Record Warning]
        F -->|Fail| I[Record Fail]
        G --> J[Continue]
        H --> J
        I --> K[Block Deployment]
    end

    subgraph "Manual Approval"
        D --> L[Identify Approvers]
        L --> M[Send Notifications]
        M --> N[Wait for Response]
        N --> O{Response?}
        O -->|Approve| P[Record Approval]
        O -->|Reject| Q[Record Rejection]
        O -->|Timeout| R[Auto Reject]
        P --> J
        Q --> K
        R --> K
    end

    subgraph "Decision"
        J --> S[Next Gate]
        K --> T[Notify Failure]
        T --> U[Raise Alert]
    end

    style C fill:#c8e6c9
    style E fill:#c8e6c9
    style D fill:#fff59d
    style P fill:#c8e6c9
    style Q fill:#ef9a9a
    style R fill:#ef9a9a
    style I fill:#ef9a9a
    style K fill:#ef9a9a
```

---

## Rollback Decision Tree

```mermaid
graph TB
    A[Deployment Issue Detected] --> B{Issue Type?}

    B -->|Health Check Fail| C[Check Pod Status]
    B -->|Error Rate High| D[Check Logs]
    B -->|Latency High| E[Check Resources]
    B -->|Crash Loop| F[Check Limits]

    C --> G{Pods Running?}
    G -->|No| H[Scale Up Deployment]
    G -->|Yes| I[Check Logs]

    D --> J{Error Pattern?}
    J -->|Transient| K[Wait & Retry]
    J -->|New Errors| L[Consider Rollback]
    J -->|Known Issues| M[Apply Hotfix]

    E --> N{Resource Limit?}
    N -->|Yes| O[Increase Resources]
    N -->|No| L

    F --> P{OOMKilled?}
    P -->|Yes| Q[Increase Memory]
    P -->|No| R[Check App Crash]

    H --> S{Fixed?}
    I --> S
    K --> S
    O --> S
    Q --> S

    S -->|No| T[Initiate Rollback]
    S -->|Yes| U[Continue Monitoring]

    L --> T
    M --> T
    R --> T

    T --> V[Rollback Deployment]
    V --> W{Rollback Success?}
    W -->|Yes| X[Stabilize & Investigate]
    W -->|No| Y[Emergency Procedures]

    U --> Z[Resume Deployment]

    X --> AA[Root Cause Analysis]
    Y --> AA

    Z --> AB[Complete Deployment]
    AA --> AC[Fix & Redeploy]

    style T fill:#ef9a9a
    style V fill:#ef9a9a
    style Y fill:#ef5350
    style X fill:#fff59d
    style Z fill:#c8e6c9
    style AB fill:#c8e6c9
```

---

## Notification Flow

```mermaid
graph TB
    subgraph "Event Sources"
        A[Deployment Event]
    end

    subgraph "Event Processing"
        A --> B{Event Type?}
        B -->|Started| C[Create Started Notification]
        B -->|Completed| D[Create Success Notification]
        B -->|Failed| E[Create Failure Notification]
        B -->|Rollback| F[Create Rollback Notification]
        B -->|Approval| G[Create Approval Request]
    end

    subgraph "Channel Routing"
        C --> H{Severity?}
        D --> H
        E --> H
        F --> H
        G --> H

        H -->|Info| I[Slack #deployments]
        H -->|Warning| J[Slack #alerts]
        H -->|Critical| K[Slack #incidents]
        H -->|Critical| L[PagerDuty Page]
        H -->|All| M[Email Recipients]
    end

    subgraph "Delivery"
        I --> N[Send Slack Message]
        J --> N
        K --> N
        L --> O[Send PagerDuty Alert]
        M --> P[Send Email]
    end

    subgraph "Tracking"
        N --> Q[Record Delivery]
        O --> Q
        P --> Q
        Q --> R[Update Audit Log]
    end

    style K fill:#ef5350
    style L fill:#ef5350
    style O fill:#ef5350
    style R fill:#f3e5f5
```

---

## Canary Deployment

```mermaid
graph TB
    subgraph "Initial State"
        A[Stable v2.9.5<br/>100% Traffic]
    end

    subgraph "Canary Phase 1"
        B[Deploy Canary v3.0.0] --> C[10% Traffic]
        C --> D[30m Monitoring]
        D --> E{Metrics OK?}
        E -->|Yes| F[Continue]
        E -->|No| G[Rollback]
    end

    subgraph "Canary Phase 2"
        F --> H[50% Traffic]
        H --> I[30m Monitoring]
        I --> J{Metrics OK?}
        J -->|Yes| K[Continue]
        J -->|No| G
    end

    subgraph "Final Phase"
        K --> L[100% Traffic]
        L --> M[Promote to Stable]
        M --> N[New Stable v3.0.0<br/>100% Traffic]
        G --> O[Rollback to v2.9.5]
    end

    style A fill:#c8e6c9
    style C fill:#fff59d
    style H fill:#fff59d
    style N fill:#c8e6c9
    style G fill:#ef9a9a
    style O fill:#ef9a9a
```

---

## Blue-Green Deployment

```mermaid
graph TB
    subgraph "Initial State"
        A[Blue: v2.9.5<br/>Active] --> B[Green: v2.9.5<br/>Inactive]
        A -.Traffic.-> A
    end

    subgraph "Deploy Phase"
        C[Deploy v3.0.0 to Green] --> D[Wait for Green Ready]
        D --> E{Green Healthy?}
        E -->|No| F[Debug & Retry]
        E -->|Yes| G[Run Tests on Green]
    end

    subgraph "Test Phase"
        G --> H{Tests Pass?}
        H -->|No| F
        H -->|Yes| I[Switch Traffic to Green]
    end

    subgraph "Switch Phase"
        I --> J[Green: v3.0.0<br/>Active] --> K[Blue: v2.9.5<br/>Inactive]
        J -.Traffic.-> J
        K --> L[Monitor 15m]
        L --> M{Issues?}
        M -->|Yes| N[Switch Back to Blue]
        M -->|No| O[Decommission Blue]
    end

    N --> P[Rollback Complete]
    O --> Q[Deployment Complete]

    style A fill:#c8e6c9
    style J fill:#c8e6c9
    style B fill:#e0e0e0
    style K fill:#e0e0e0
    style N fill:#ef9a9a
    style Q fill:#a5d6a7
```

---

## Disaster Recovery

```mermaid
graph TB
    subgraph "Disaster Detection"
        A[Health Check Failure] --> B{Severity?}
        B -->|Single Service| C[P2 Incident]
        B -->|Partial Outage| D[P1 Incident]
        B -->|Complete Outage| E[P0 Critical]
    end

    subgraph "P0 Response"
        E --> F[Page All Executives]
        F --> G[Activate War Room]
        G --> H{Primary Region Recoverable?}
        H -->|Yes| I[Attempt Recovery]
        H -->|No| J[Activate DR Region]
    end

    subgraph "DR Activation"
        J --> K[Failover DNS to DR]
        K --> L[Scale DR Resources]
        L --> M[Update Status Page]
        M --> N[Verify DR Operations]
    end

    subgraph "Recovery"
        I --> O{Recovery Successful?}
        O -->|Yes| P[Resume Operations]
        O -->|No| J
        N --> Q[Stabilize in DR]
    end

    subgraph "Post-Incident"
        P --> R[Post-Mortem]
        Q --> R
        R --> S[Update Runbooks]
        S --> T[Prevent Recurrence]
    end

    subgraph "P1 Response"
        D --> U[Page On-Call]
        U --> V[Assess Impact]
        V --> W[Implement Mitigation]
        W --> X[Monitor Recovery]
    end

    subgraph "P2 Response"
        C --> Y[Create Ticket]
        Y --> Z[Investigate]
        Z --> AA[Fix & Deploy]
    end

    style E fill:#ef5350
    style F fill:#ef5350
    style J fill:#ef5350
    style K fill:#ef5350
    style D fill:#ff9800
    style U fill:#ff9800
    style C fill:#ffa726
```

---

## Event Sequence Diagram

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant Git as GitHub
    participant CI as CI/CD
    participant QA as Quality Gates
    participant App as Approver
    participant K8s as Kubernetes
    participant Mon as Monitoring
    participant Notify as Notifications

    Dev->>Git: Push Code
    Git->>CI: Trigger Workflow
    CI->>CI: Build & Test
    CI->>QA: Run Quality Gates
    QA->>QA: Compile, Dialyzer, Xref
    QA->>QA: Test Coverage
    QA->>QA: Security Scan
    QA-->>CI: Gates Passed

    CI->>App: Request Approval
    App->>App: Review Changes
    App->>CI: Approve Deployment

    CI->>K8s: Deploy to Staging
    K8s->>K8s: Create Pods
    K8s->>Mon: Expose Metrics

    Mon->>Mon: Health Checks
    Mon->>Mon: Soak Period (24h)
    Mon-->>CI: Stable

    CI->>App: Request Production Approval
    App->>CI: Approve

    CI->>K8s: Deploy to Production
    K8s->>K8s: Canary (10%)
    K8s->>Mon: Canary Metrics

    Mon->>Mon: Analyze Canary
    Mon-->>CI: Canary Pass

    CI->>K8s: Full Rollout
    K8s->>K8s: Update All Pods

    Mon->>Mon: Verify Health
    Mon-->>Notify: Send Success
    Notify->>Dev: Deployment Complete

    Note over CI,K8s: If any step fails:
    CI->>Notify: Send Failure Alert
    CI->>K8s: Automatic Rollback
```

---

## Environment Promotion Flow

```mermaid
graph LR
    subgraph "Code Flow"
        A[Feature Branch] --> B[PR to Main]
        B --> C[Merge to Main]
    end

    subgraph "Deployment Flow"
        C --> D[Auto Deploy to Dev]
        D --> E[Dev Tests Pass?]
        E -->|Yes| F[Auto Deploy to QA]
        E -->|No| G[Fix & Retry]
        F --> H[QA Tests Pass?]
        H -->|Yes| I[Auto Deploy to Staging]
        H -->|No| G
        I --> J[Staging Soak 24h]
        J --> K{Stable?}
        K -->|Yes| L[Tag Release]
        K -->|No| M[Investigate]
        L --> N[Request Production Approval]
        N --> O[Approved?]
        O -->|Yes| P[Deploy to Production]
        O -->|No| Q[Reject]
        P --> R[Canary Rollout]
        R --> S[Full Production]
    end

    subgraph "Rollback Paths"
        S --> T{Issues?}
        T -->|Yes| U[Rollback]
        T -->|No| V[Complete]
        U --> W[Investigate]
    end

    style D fill:#c8e6c9
    style F fill:#c8e6c9
    style I fill:#fff59d
    style P fill:#fff59d
    style S fill:#ef9a9a
    style U fill:#ef5350
    style V fill:#a5d6a7
```

---

## How to Render These Diagrams

### Using Mermaid Live Editor
1. Copy the diagram code
2. Visit https://mermaid.live
3. Paste the code
4. Export as PNG/SVG

### Using VS Code
1. Install Mermaid Preview extension
2. Open this file
3. Right-click -> "Mermaid: Open Preview"

### Using GitHub
1. Commit this file to your repo
2. GitHub will render Mermaid diagrams automatically

### Using CLI
```bash
# Install mermaid-cli
npm install -g @mermaid-js/mermaid-cli

# Render diagram
mmdc -i diagrams.md -o output.png
```
