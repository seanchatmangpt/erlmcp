# C4 Level 2: Container Diagram

```mermaid
flowchart TB
    subgraph taiea[TAIEA Umbrella Apps]
        governor[taiea_governor\n(policy)]
        receipts[taiea_receipts\n(audit + lineage)]
    end

    subgraph tcps[TCPS Pipeline]
        workorders[tcps_work_order]
        heijunka[tcps_heijunka]
        shacl[tcps_shacl_validator]
        receiptsvc[tcps_receipt + tcps_receipt_verifier]
        quality[tcps_quality_gates]
        andon[tcps_andon]
    end

    subgraph erlmcp_app[erlmcp OTP Application]
        sup[erlmcp_sup\n(supervision tree)]
        registry[erlmcp_registry\n(gproc router)]
        serverSup[erlmcp_server_sup]
        clientSup[erlmcp_client_sup]
        transportSup[erlmcp_transport_sup]
        tracing[erlmcp_tracing\n(OpenTelemetry)]
        recovery[erlmcp_recovery_manager]
        health[erlmcp_health_monitor]
    end

    transportSup --> transports[[Transports
    - stdio (erlmcp_transport_stdio_new)
    - tcp (erlmcp_transport_tcp)
    - http (erlmcp_transport_http)
    ]]

    serverSup --> servers[[Server instances]]
    clientSup --> clients[[Client instances]]
    registry --> transports
    registry --> servers

    workorders --> heijunka --> shacl --> quality --> receiptsvc
    receiptsvc --> andon

    tcps -->|Receipts / policies| erlmcp_app
    transports -->|Telemetry| tracing
    tracing --> recovery
    tracing --> health

    taiea --> recovery
    taiea --> health

    transports --> registry
    servers --> registry

    classDef note fill:#f7f7f7,stroke:#999,color:#333;
    note0("Use this container view to align platform, TCPS, and TAIEA teams.")
    note0 --- erlmcp_app
```
