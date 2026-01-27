# MCP HTTP Header Validation

## Overview

The `erlmcp_http_headers` module implements comprehensive HTTP header validation per the MCP (Model Context Protocol) specification. It provides validation for protocol versions, content negotiation, HTTP methods, and response header formatting.

## Key Features

### 1. MCP-Protocol-Version Validation

Validates the `MCP-Protocol-Version` header against supported protocol versions:

- **2025-11-25** (Latest)
- **2024-11-05** (Stable)

**Default behavior:** Uses version `2024-11-05` if the header is not specified.

```erlang
%% Valid header
Headers = [{<<"mcp-protocol-version">>, <<"2025-11-25">>}],
{ok, <<"2025-11-25">>} = erlmcp_http_headers:validate_protocol_version(Headers).

%% Missing header defaults to 2024-11-05
{ok, <<"2024-11-05">>} = erlmcp_http_headers:validate_protocol_version([]).

%% Invalid version rejected
{error, {unsupported_protocol_version, _}} =
    erlmcp_http_headers:validate_protocol_version([{<<"mcp-protocol-version">>, <<"2023-01-01">>}]).
```

### 2. Accept Header Validation & Content Negotiation

Validates the `Accept` header and determines the response format:

**Supported formats:**
- `application/json` → JSON responses (status 200)
- `text/event-stream` → Server-Sent Events (status 202)
- `*/*` → Defaults to JSON
- `application/*` → Defaults to JSON
- `text/*` → Defaults to SSE

**Default behavior:** Uses JSON format if the Accept header is not specified.

```erlang
%% Request JSON
Headers = [{<<"accept">>, <<"application/json">>}],
{ok, json} = erlmcp_http_headers:validate_accept_header(Headers).

%% Request SSE
Headers = [{<<"accept">>, <<"text/event-stream">>}],
{ok, sse} = erlmcp_http_headers:validate_accept_header(Headers).

%% Multiple types - uses first supported
Headers = [{<<"accept">>, <<"application/json, text/plain">>}],
{ok, json} = erlmcp_http_headers:validate_accept_header(Headers).

%% Unsupported content type
{error, {unsupported_content_type, _}} =
    erlmcp_http_headers:validate_accept_header([{<<"accept">>, <<"application/xml">>}]).
```

### 3. HTTP Method Validation

Validates HTTP methods with special handling for DELETE (not allowed in MCP):

**Allowed methods:**
- GET
- POST
- PUT
- PATCH
- HEAD
- OPTIONS

**Disallowed methods:**
- DELETE → Returns `{error, method_not_allowed}`

```erlang
%% Valid method
{ok, <<"GET">>} = erlmcp_http_headers:validate_method(<<"GET">>).

%% Method as atom (lowercase converted to uppercase)
{ok, <<"POST">>} = erlmcp_http_headers:validate_method(post).

%% DELETE not allowed
{error, method_not_allowed} = erlmcp_http_headers:validate_method(<<"DELETE">>).

%% Invalid method
{error, invalid_method} = erlmcp_http_headers:validate_method(<<"INVALID">>).
```

### 4. HTTP Status Code Mapping

Maps response types to appropriate HTTP status codes:

| Response Type | Status Code | Use Case |
|---------------|-------------|----------|
| `ok` | 200 | Successful single response |
| `accepted` | 202 | Response to notification/streaming |
| `bad_request` | 400 | Invalid headers, missing fields |
| `forbidden` | 403 | Origin validation failed |
| `not_found` | 404 | Session expired or not found |
| `method_not_allowed` | 405 | DELETE or unsupported method |
| `error` | 500 | Internal server error |

```erlang
200 = erlmcp_http_headers:get_http_status_code(ok).
202 = erlmcp_http_headers:get_http_status_code(accepted).
400 = erlmcp_http_headers:get_http_status_code(bad_request).
```

### 5. Content-Type Response Headers

Automatically sets the correct `Content-Type` header based on response format:

| Format | Content-Type |
|--------|--------------|
| `json` | `application/json` |
| `sse` | `text/event-stream` |
| `error` | `text/plain` |

```erlang
<<"application/json">> = erlmcp_http_headers:get_response_content_type(json).
<<"text/event-stream">> = erlmcp_http_headers:get_response_content_type(sse).
<<"text/plain">> = erlmcp_http_headers:get_response_content_type(error).
```

### 6. Header Extraction

Case-insensitive header extraction from list of tuples:

```erlang
Headers = [
    {<<"Content-Type">>, <<"application/json">>},
    {<<"ACCEPT">>, <<"text/plain">>}
],

%% Case insensitive lookup
{ok, <<"application/json">>} = erlmcp_http_headers:extract_header(<<"content-type">>, Headers).
{ok, <<"text/plain">>} = erlmcp_http_headers:extract_header(<<"accept">>, Headers).

%% Missing header
{error, not_found} = erlmcp_http_headers:extract_header(<<"authorization">>, Headers).
```

### 7. Response Header Formatting

Formats complete response headers with automatic Content-Type setting:

```erlang
%% JSON response with extra headers
ExtraHeaders = [{<<"x-request-id">>, <<"abc123">>}],
Headers = erlmcp_http_headers:format_response_headers(json, ExtraHeaders),
%% Returns: [{<<"content-type">>, <<"application/json">>}, {<<"x-request-id">>, <<"abc123">>}]

%% SSE response without extra headers
Headers = erlmcp_http_headers:format_response_headers(sse, []),
%% Returns: [{<<"content-type">>, <<"text/event-stream">>}]
```

## API Reference

### validate_protocol_version/1
```erlang
-spec validate_protocol_version(headers()) -> {ok, binary()} | {error, term()}.
```
Validates MCP-Protocol-Version header and returns the version to use.

### validate_accept_header/1
```erlang
-spec validate_accept_header(headers()) -> {ok, response_format()} | {error, term()}.
```
Validates Accept header and determines response format (json | sse).

### get_response_content_type/1
```erlang
-spec get_response_content_type(response_format()) -> binary().
```
Returns the Content-Type header value for the given response format.

### get_http_status_code/1
```erlang
-spec get_http_status_code(atom()) -> http_status().
```
Returns the HTTP status code for the given response type.

### validate_method/1
```erlang
-spec validate_method(term()) -> {ok, binary()} | {error, term()}.
```
Validates HTTP method (rejects DELETE, accepts GET/POST/PUT/PATCH/HEAD/OPTIONS).

### extract_header/2
```erlang
-spec extract_header(header_name(), headers()) -> {ok, header_value()} | {error, term()}.
```
Extracts header value with case-insensitive name matching.

### format_response_headers/2
```erlang
-spec format_response_headers(response_format(), headers()) -> [{binary(), binary()}].
```
Formats complete response headers with automatic Content-Type setting.

## Implementation Notes

### Header Normalization
- Header names are normalized to lowercase for comparison
- Header values are converted to binary format
- Both list-based headers `[{name, value}]` and map-based headers `#{name => value}` are supported

### Default Behavior
- Missing `MCP-Protocol-Version` header defaults to `2024-11-05`
- Missing `Accept` header defaults to `application/json`
- Unknown Accept types return an error (fail-safe approach)

### Quality Factors
The implementation handles quality factors in Accept headers (e.g., `application/json;q=0.9`), treating them as part of the media type string and extracting the base type for validation.

### Case Insensitivity
All header names are case-insensitive:
- `Accept`, `ACCEPT`, `accept` all match
- `MCP-Protocol-Version`, `mcp-protocol-version` all match

## Testing

The module includes a comprehensive test suite with 60 tests covering:
- Protocol version validation (7 tests)
- Accept header validation (9 tests)
- HTTP status code mapping (8 tests)
- HTTP method validation (11 tests)
- Content-Type response headers (4 tests)
- Header extraction (6 tests)
- Error handling (5 tests)
- Case insensitivity (4 tests)
- Response header formatting (6 tests)

Run tests with:
```bash
rebar3 eunit --module=erlmcp_http_headers_tests
```

## Integration with MCP Handlers

When implementing HTTP handlers with Cowboy or other web servers:

```erlang
handle_request(Req, State) ->
    Headers = cowboy_req:headers(Req),

    %% Validate protocol version
    case erlmcp_http_headers:validate_protocol_version(Headers) of
        {ok, Version} ->
            %% Validate content negotiation
            case erlmcp_http_headers:validate_accept_header(Headers) of
                {ok, Format} ->
                    %% Process request with appropriate format
                    process_request(Req, Format, State);
                {error, _Reason} ->
                    %% Bad request - unsupported content type
                    ResponseHeaders =
                        erlmcp_http_headers:format_response_headers(error, []),
                    cowboy_req:reply(400, ResponseHeaders,
                        <<"Unsupported content type">>, Req)
            end;
        {error, {unsupported_protocol_version, _Version}} ->
            %% Bad request - unsupported MCP version
            ResponseHeaders =
                erlmcp_http_headers:format_response_headers(error, []),
            cowboy_req:reply(400, ResponseHeaders,
                <<"Unsupported MCP protocol version">>, Req)
    end.
```

## Future Enhancements

1. **CORS Header Support** - Add validation for CORS-related headers
2. **Authorization Header Parsing** - Helper functions for Bearer token extraction
3. **Quality Factor Sorting** - Sort multiple Accept types by quality factor
4. **Custom Header Registration** - Allow registration of custom header validators
5. **Header Compression** - Support for compression-related headers

## References

- [MCP Specification](https://spec.modelcontextprotocol.io/)
- [HTTP/1.1 Header Fields](https://tools.ietf.org/html/rfc7231#section-3.1)
- [Server-Sent Events (SSE)](https://html.spec.whatwg.org/multipage/server-sent-events.html)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
