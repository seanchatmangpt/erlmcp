# Feature Diagram: Icon Metadata & Validation

```mermaid
graph TB
    addResource[erlmcp_server:add_resource]
    iconValidator[erlmcp_icon_validator]
    iconStore[Icon Storage (ETS / priv/icons)]
    responseBuilder[JSON-RPC Responses]

    addResource --> iconValidator
    iconValidator --> iconStore
    addResource --> resourceTable[Resource Registry]
    responseBuilder --> iconValidator
```

```mermaid
sequenceDiagram
    participant Author
    participant Server
    participant IconValidator
    participant Storage

    Author->>Server: add_resource(..., #{icon => #{src => "https://..."}})
    Server->>IconValidator: validate(icon)
    IconValidator->>IconValidator: check_scheme, check_mime, same-origin?
    alt valid
        IconValidator->>Storage: store metadata / cached asset
        Storage-->>IconValidator: ok
        IconValidator-->>Server: ok
        Server->>Author: ok
    else invalid
        IconValidator-->>Server: {error, invalid_icon}
        Server-->>Author: error
    end

    client->>Server: resources/list
    Server->>Storage: fetch icon metadata
    Storage-->>Server: icon info
    Server-->>client: response + icon
```
