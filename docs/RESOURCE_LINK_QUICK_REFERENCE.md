# Resource Link Content Type - Quick Reference

## Gap #33 Implementation Quick Reference

### 1. Constants

```erlang
-include("erlmcp.hrl").

% Content type
?MCP_CONTENT_TYPE_RESOURCE_LINK  % <<"resource/link">>

% Field names
?MCP_RESOURCE_LINK_FIELD_TYPE     % <<"type">>
?MCP_RESOURCE_LINK_FIELD_URI      % <<"uri">>
?MCP_RESOURCE_LINK_FIELD_NAME     % <<"name">>
?MCP_RESOURCE_LINK_FIELD_MIME_TYPE % <<"mimeType">>
?MCP_RESOURCE_LINK_FIELD_SIZE     % <<"size">>
```

### 2. Record Type

```erlang
#mcp_resource_link{
    uri :: binary(),
    name :: binary() | undefined,
    mime_type :: binary() | undefined,
    size :: integer() | undefined
}
```

### 3. API Functions

#### Encoding

```erlang
%% Basic encoding (URI + MIME type only)
erlmcp_server:encode_resource_link(Uri, MimeType) -> map()

%% Full encoding (with optional name and size)
erlmcp_server:encode_resource_link(Uri, MimeType, Name, Size) -> map()
```

#### Validation

```erlang
%% Validate URI
erlmcp_server:validate_resource_link_uri(Uri) -> {ok, Uri} | {error, invalid_uri}
```

### 4. Common Usage Patterns

#### Pattern 1: Simple Link

```erlang
#mcp_resource_link{
    uri = <<"https://example.com/data.csv">>,
    mime_type = <<"text/csv">>
}
```

#### Pattern 2: Link with Metadata

```erlang
#mcp_resource_link{
    uri = <<"https://example.com/data.json">>,
    mime_type = <<"application/json">>,
    name = <<"JSON Data">>,
    size = 2048
}
```

#### Pattern 3: Content with Link

```erlang
#mcp_content{
    type = <<"text">>,
    text = <<"See linked resource">>,
    mime_type = <<"text/plain">>,
    resource_link = #mcp_resource_link{
        uri = <<"https://example.com/ref.pdf">>,
        mime_type = <<"application/pdf">>,
        name = <<"Reference">>,
        size = 1024000
    }
}
```

### 5. Supported URI Schemes

| Scheme | Example | Use Case |
|--------|---------|----------|
| `http://` | `http://example.com/data` | HTTP URLs |
| `https://` | `https://example.com/data` | HTTPS URLs (preferred) |
| `ftp://` | `ftp://ftp.example.com/file` | FTP protocol |
| `file://` | `file:///path/to/file` | Local files |
| `resource://` | `resource://app/resource` | MCP resources |
| `data:` | `data:application/json,...` | Data URIs |
| `/` | `/home/user/file` | Absolute paths |
| `./` | `./resources/file` | Relative paths |
| `../` | `../config/file` | Parent paths |

### 6. JSON-RPC Response Format

```json
{
  "type": "resource/link",
  "uri": "https://example.com/resource",
  "mimeType": "application/json",
  "name": "Resource Name",
  "size": 1024
}
```

### 7. MIME Types (Common Examples)

```
text/plain
text/html
text/markdown
application/json
application/pdf
application/zip
image/png
image/jpeg
video/mp4
audio/mpeg
```

### 8. Integration Points

#### In Resource Handlers

```erlang
handle_resource(Uri) ->
    #mcp_content{
        type = <<"text">>,
        text = <<"Main content">>,
        resource_link = #mcp_resource_link{
            uri = <<"https://api.example.com", Uri/binary>>,
            mime_type = <<"application/json">>,
            name = <<"Full Data">>,
            size = 5000000
        }
    }.
```

#### In Tool Results

```erlang
ToolResult = [
    #{
        <<"type">> => <<"text">>,
        <<"text">> => <<"Result">>,
        <<"resourceLink">> => #{
            <<"type">> => <<"resource/link">>,
            <<"uri">> => <<"https://example.com/result.pdf">>,
            <<"mimeType">> => <<"application/pdf">>
        }
    }
].
```

#### In Prompts

```erlang
PromptMessage = #{
    <<"role">> => <<"user">>,
    <<"content">> => [
        #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"See reference">>
        },
        #{
            <<"type">> => <<"resource/link">>,
            <<"uri">> => <<"https://docs.example.com">>,
            <<"mimeType">> => <<"text/html">>
        }
    ]
}.
```

### 9. Validation Examples

```erlang
%% Valid URIs
erlmcp_server:validate_resource_link_uri(<<"https://example.com">>).
% {ok, <<"https://example.com">>}

erlmcp_server:validate_resource_link_uri(<<"./path/to/file">>).
% {ok, <<"./path/to/file">>}

erlmcp_server:validate_resource_link_uri(<<"resource://app/data">>).
% {ok, <<"resource://app/data">>}

%% Invalid URIs
erlmcp_server:validate_resource_link_uri(<<"">>).
% {error, invalid_uri}

erlmcp_server:validate_resource_link_uri(<<"invalid">>).
% {error, invalid_uri}
```

### 10. Encoding Examples

```erlang
%% Basic encoding
Map1 = erlmcp_server:encode_resource_link(
    <<"https://example.com/data.json">>,
    <<"application/json">>
).
% #{
%   <<"type">> => <<"resource/link">>,
%   <<"uri">> => <<"https://example.com/data.json">>,
%   <<"mimeType">> => <<"application/json">>
% }

%% Full encoding
Map2 = erlmcp_server:encode_resource_link(
    <<"https://example.com/file.zip">>,
    <<"application/zip">>,
    <<"Archive">>,
    5242880
).
% #{
%   <<"type">> => <<"resource/link">>,
%   <<"uri">> => <<"https://example.com/file.zip">>,
%   <<"mimeType">> => <<"application/zip">>,
%   <<"name">> => <<"Archive">>,
%   <<"size">> => 5242880
% }
```

### 11. Size Guidelines

| File Type | Typical Size | Size Bytes |
|-----------|--------------|-----------|
| Small text file | 1 KB | 1,024 |
| Medium document | 100 KB | 102,400 |
| PDF document | 1 MB | 1,048,576 |
| Video clip | 50 MB | 52,428,800 |
| Large dataset | 500 MB | 524,288,000 |
| Large video | 1 GB | 1,073,741,824 |

### 12. Error Handling

```erlang
%% Validate before encoding
case erlmcp_server:validate_resource_link_uri(Uri) of
    {ok, ValidUri} ->
        Link = erlmcp_server:encode_resource_link(ValidUri, MimeType),
        {ok, Link};
    {error, invalid_uri} ->
        {error, invalid_uri_format}
end.
```

### 13. Best Practices

1. **Always validate URIs** before encoding
2. **Prefer HTTPS** for security
3. **Include metadata** (name, size) when available
4. **Use standard MIME types** for consistency
5. **Keep URIs stable** for long-term links
6. **Document external resources** in comments
7. **Handle missing links gracefully**
8. **Cache link metadata** when possible

### 14. Testing

```erlang
%% Run tests
rebar3 eunit --module=erlmcp_resource_link_tests

%% Expected output
%% 20 tests passed
```

### 15. Performance Notes

- **Encoding**: O(1) constant time
- **Validation**: O(1) constant time
- **Memory**: Minimal footprint
- **Network**: No additional overhead

---

**Specification**: MCP 2025-11-25
**Gap**: #33 - Resource Link Content Type
**Status**: COMPLETE ✅
**Last Updated**: 2026-01-27
