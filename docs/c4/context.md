# C4 Level 1: System Context

```mermaid
flowchart LR
    marketplace([Marketplace Demand Signals\n(pull-based work)])
    taiea([TAIEA Governance Umbrella\n(policy + recovery)])
    clients([MCP Clients/Servers\n(resources, tools, prompts)])
    auditors([Receipts / Auditing Systems\n(Andon, compliance, stakeholders)])

    subgraph erlmcp[erlmcp + TCPS Factory]
        direction TB
        core["erlmcp Runtime\n(supervisors, registry, transports)"]
        tcps["TCPS Pipeline\n(work orders, SHACL, receipts)"]
    end

    marketplace -->|Work orders| tcps
    taiea -->|Governance policies / recovery| core
    clients -->|JSON-RPC / transports| core
    core -->|Receipts, Andon events| tcps
    tcps -->|Evidence| auditors
    auditors -->|Feedback / Kaizen insights| tcps

    classDef note fill:#f7f7f7,stroke:#999,color:#333;
    note0("Use this context view when onboarding stakeholders or explaining the erlmcp+TCPS factory within TAIEA.")
    note0 --- erlmcp
```
