# C4 Level 3: Transport Subsystem Components

```mermaid
flowchart LR
    subgraph supervisor[erlmcp_transport_sup]
        direction TB
        start[transport_module resolver]
    end

    start --> stdio[erlmcp_transport_stdio_new\n(stdio behaviour)]
    start --> tcp[erlmcp_transport_tcp\n(ranch protocol)]
    start --> http[erlmcp_transport_http\n(gun client)]

    stdio -->|route_to_server| registry[erlmcp_registry (gproc)]
    tcp -->|route_to_server| registry
    http -->|route_to_server| registry

    registry --> servers[erlmcp_server instances]
    servers --> registry

    stdio --> health[erlmcp_health_monitor]
    tcp --> health
    http --> health

    stdio --> recovery[erlmcp_recovery_manager]
    tcp --> recovery
    http --> recovery

    tcp --> ranch[ranch listener / ranch_protocol]
    http --> gun[gun HTTP2 client]
    stdio --> osstdio[STDIN/STDOUT reader]

    transportsClients[MCP Clients/Servers] --> tcp
    transportsClients --> http
    transportsClients --> stdio

    classDef note fill:#f7f7f7,stroke:#999,color:#333;
    note0("Use this component view during transport refactor discussions or when onboarding engineers to the routing behaviour.")
    note0 --- supervisor
```
