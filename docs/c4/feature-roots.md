# Feature Diagram: Roots Enforcement

```mermaid
flowchart TB
    client[MCP Client]
    server[erlmcp_server]
    rootsMod[erlmcp_roots]
    etsRoots[ETS: roots_table]
    fsOps[Resource Handler (files)]

    client -->|resources/read| server
    server --> rootsMod
    rootsMod --> etsRoots
    rootsMod -->|validate_path| fsOps
    fsOps --> server --> client

    subgraph Roots Management
        adminOps[Work orders / config]
        adminOps --> rootsMod
        rootsMod --> watchers[fs watchers]
    end
```

```mermaid
sequenceDiagram
    participant Client
    participant Server
    participant Roots
    participant FileHandler

    Client->>Server: resources/read {uri=file://...
    Server->>Roots: validate(uri)
    Roots->>Roots: canonicalize(path)
    Roots->>Roots: check_allowed(ETS)
    alt allowed
        Roots-->>Server: ok
        Server->>FileHandler: read(path)
        FileHandler-->>Server: content
        Server-->>Client: result
    else denied
        Roots-->>Server: {error, forbidden}
        Server-->>Client: error
    end
```
