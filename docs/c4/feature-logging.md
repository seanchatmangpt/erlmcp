# Feature Diagram: Logging Control (`logging/setLevel`)

```mermaid
flowchart LR
    client[MCP Client]
    server[erlmcp_server]
    loggingMod[erlmcp_logging]
    logger[OTP logger / lager]

    client -->|logging/setLevel| server
    server --> loggingMod --> logger
    logger --> telem[Telemetry (OpenTelemetry)]
```

```mermaid
sequenceDiagram
    participant Client
    participant Server
    participant Logging
    participant Logger

    Client->>Server: logging/setLevel {level="warning"}
    Server->>Logging: set_level(level)
    Logging->>Logger: logger:set_primary_config(level)
    Logger-->>Logging: ok
    Logging-->>Server: ok
    Server-->>Client: {result, ok}
```
