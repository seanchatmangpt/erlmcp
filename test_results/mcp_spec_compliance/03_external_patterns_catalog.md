# MCP External Patterns Catalog

## Research Summary

This document catalogs implementation patterns, testing approaches, and best practices gathered from official MCP SDKs, specification repositories, and community implementations. The catalog focuses on actionable patterns that can be applied to the erlmcp implementation.

---

## 1. Testing Patterns Catalog

### 1.1 Official SDK Testing Patterns

#### Python SDK Patterns
**Source**: [modelcontextprotocol/python-sdk](https://github.com/modelcontextprotocol/python-sdk)

```python
# Pattern: In-memory transport testing
class TestInMemoryTransport:
    """Test MCP servers using in-memory transport instead of real sockets"""
    def setup(self):
        self.transport = StdioTransport()
        self.server = McpServer(self.transport)

    def test_tool_execution(self):
        # Test tool execution through transport interface
        request = {"jsonrpc": "2.0", "method": "tools/call", "params": {...}}
        response = self.server.handle_message(request)
        assert response.get("result") is not None
```

**Key Patterns:**
- **Transport-agnostic testing**: Test servers through transport interfaces, not direct module calls
- **Message-based assertions**: Validate JSON-RPC message structure and content
- **Capability validation**: Test server capabilities before tool execution
- **Error scenario testing**: Test network failures, timeouts, and malformed messages

#### TypeScript SDK Patterns
**Source**: [modelcontextprotocol/typescript-sdk](https://github.com/modelcontextprotocol/typescript-sdk)

```typescript
// Pattern: Client-server integration testing
describe('MCP Integration Tests', () => {
  let client: McpClient;
  let server: McpServer;

  beforeEach(() => {
    // Setup test server with mock transport
    server = new McpServer(new TestTransport());
    client = new McpClient(server);
  });

  test('Tool call with streaming', async () => {
    const stream = await client.callTool('echo', { text: 'test' });
    const result = await collectStream(stream);
    expect(result).toBe('test');
  });
});
```

**Key Patterns:**
- **Streaming test patterns**: Test async operations with streams
- **Client-server integration**: Test complete request-response cycles
- **Capability negotiation**: Test resource/tool availability checking
- **Progress token handling**: Test streaming responses with progress tracking

### 1.2 Conformance Testing Patterns

**Source**: [modelcontextprotocol/conformance](https://github.com/modelcontextprotocol/conformance)

```python
# Pattern: Conformance test suite
class ConformanceTestSuite:
    """Official conformance test suite implementation"""

    def test_server_capabilities(self):
        """Test server capabilities compliance"""
        response = client.send_request("server/listTools")
        assert response['jsonrpc'] == '2.0'
        assert 'result' in response
        assert 'tools' in response['result']

    def test_protocol_compliance(self):
        """Test JSON-RPC 2.0 compliance"""
        response = client.send_request("tools/call", invalid_params)
        assert response['jsonrpc'] == '2.0'
        assert 'error' in response
        assert response['error']['code'] == -32602  # Invalid params
```

**Conformance Testing Requirements:**
- **Message format validation**: All messages must be valid JSON-RPC 2.0
- **Method naming conventions**: Follow MCP method naming (tools/call, resources/read)
- **Error code compliance**: Use standard JSON-RPC error codes
- **Transport independence**: Test over all supported transports

### 1.3 Third-Party Testing Frameworks

**Source**: [@mcp-testing/server-tester](https://www.npmjs.com/package/@mcp-testing/server-tester)

```javascript
// Pattern: Playwright-based end-to-end testing
describe('MCP Server E2E Tests', () => {
  test('Complete workflow', async () => {
    // Launch MCP server
    const server = startMcpServer();

    // Connect via HTTP transport
    const client = await connectMcpHttp(server.url);

    // Execute complete tool call workflow
    const result = await client.callTool('filesystem/read', {
      path: '/test/file.txt'
    });

    // Validate result structure
    expect(result).toHaveProperty('content');
    expect(result.content).toContain('file content');
  });
});
```

**E2E Testing Patterns:**
- **Real transport testing**: Test with actual HTTP/WebSocket transports
- **Integration scenarios**: Test complete user workflows
- **Performance testing**: Measure response times and throughput
- **Error recovery**: Test automatic reconnection and error handling

---

## 2. Security Implementation Guide

### 2.1 OAuth 2.0 Implementation Patterns

**Sources**:
- [SEP-985: OAuth 2.0 Protected Resource Metadata](https://mcp-docs.cn/community/seps/985-align-oauth-20-protected-resource-metadata-with-rf)
- [MCP Authorization Specification](https://modelcontextprotocol.io/specification/draft/basic/authorization/)

#### OAuth 2.1 Protocol Implementation

```erlang
% Pattern: OAuth 2.1 authentication flow
-spec authenticate_oauth21(Headers :: map(), Params :: map()) ->
    {ok, User :: binary()} | {error, Reason :: term()}.

authenticate_oauth21(Headers, Params) ->
    % Extract bearer token from Authorization header
    AuthHeader = maps:get(<<"authorization">>, Headers, <<>>),
    case parse_bearer_token(AuthHeader) of
        {ok, Token} ->
            % Validate token with OAuth 2.1
            case oauth21:validate_token(Token) of
                {ok, Claims} ->
                    {ok, maps:get(<<"sub">>, Claims)};
                {error, invalid_token} ->
                    {error, unauthorized}
            end;
        {error, malformed} ->
            {error, bad_request}
    end.
```

**Key OAuth 2.1 Requirements:**
- **PKCE support**: Proof Key for Code Exchange for public clients
- **Token introspection**: Validate access tokens with authorization server
- **Scope validation**: Validate requested scopes against token scopes
- **Secure token storage**: Store tokens securely with appropriate expiration

#### Dynamic Client Registration

```erlang
% Pattern: RFC 7591 Dynamic Client Registration
-spec register_client(ClientInfo :: map()) ->
    {ok, ClientRegistrationResponse :: map()} | {error, Reason :: term()}.

register_client(ClientInfo) ->
    % Validate client metadata
    case validate_client_metadata(ClientInfo) of
        {ok, Validated} ->
            % Generate client credentials
            ClientId = generate_client_id(),
            ClientSecret = generate_client_secret(),

            % Store client configuration
            case mcp_auth:store_client(ClientId, ClientSecret, Validated) of
                ok ->
                    Response = #{
                        <<"client_id">> => ClientId,
                        <<"client_secret">> => ClientSecret,
                        <<"client_id_issued_at">> => timestamp(),
                        <<"client_secret_expires_at">> => expiration_time()
                    },
                    {ok, Response};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, ValidationErrors} ->
            {error, {invalid_client_metadata, ValidationErrors}}
    end.
```

### 2.2 SEP-835: Resource Metadata Alignment

**Implementation Pattern**:

```erlang
% Pattern: OAuth 2.0 Protected Resource Metadata
-spec get_resource_metadata(Uri :: binary()) ->
    {ok, Metadata :: map()} | {error, not_found}.

get_resource_metadata(Uri) ->
    case mcp_resources:get_resource_config(Uri) of
        {ok, ResourceConfig} ->
            % RFC 9728 compliant metadata
            Metadata = #{
                <<"resource_uri">> => Uri,
                <<"token_endpoint">> => <<"https://auth.example.com/token">>,
                <<"authorization_endpoint">> => <<"https://auth.example.com/authorize">>,
                <<"scopes_supported">> => [<<"read">>, <<"write">>],
                <<"response_types_supported">> => [<<"code">>, <<"token">>],
                <<"grant_types_supported">> => [<<"authorization_code">>, <<"client_credentials">>]
            },
            {ok, Metadata};
        {error, not_found} ->
            {error, not_found}
    end.
```

### 2.3 SEP-991: URL-based Client Registration

**Implementation Pattern**:

```erlang
% Pattern: URL-based client registration
-spec register_client_from_url(Url :: binary()) ->
    {ok, Registration :: map()} | {error, Reason :: term()}.

register_client_from_url(Url) ->
    % Fetch client configuration from URL
    case http_client:get(Url) of
        {ok, 200, ResponseBody} ->
            ClientConfig = jsx:decode(ResponseBody, [{labels, atom}]),
            % Validate client configuration
            case validate_remote_client_config(ClientConfig) of
                {ok, Validated} ->
                    register_client(Validated);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, StatusCode, _} ->
            {error, {invalid_url_response, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.
```

### 2.4 Security Best Practices

**Source**: [Security Best Practices](https://modelcontextprotocol.io/specification/draft/basic/security_best_practices)

1. **Transport Security**
   - Always use TLS for HTTP/WebSocket transports
   - Validate CORS headers for web-based clients
   - Implement proper origin validation

2. **Input Validation**
   - Validate all JSON-RPC parameters against schemas
   - Sanitize user inputs to prevent injection attacks
   - Use jesse for JSON schema validation

3. **Error Handling**
   - Never expose sensitive information in error messages
   - Use generic error messages for security-related errors
   - Implement rate limiting to prevent brute force attacks

---

## 3. Performance Baseline Targets

### 3.1 Latency Benchmarks

**Source**: Research on MCP performance metrics

```erlang
% Pattern: Performance monitoring
-spec record_latency(Method :: binary(), Duration :: pos_integer()) -> ok.

record_latency(Method, Duration) ->
    % Record latency metrics
    Metrics = #{
        method => Method,
        duration_us => Duration,
        timestamp => erlang:system_time(microsecond),
        transport => current_transport()
    },
    erlmcp_metrics:record('latency_p50_us', Duration),
    erlmcp_metrics:record('latency_p95_us', Duration),
    erlmcp_metrics:record('latency_p99_us', Duration).
```

**Target Performance Metrics:**
- **P50 Latency**: < 300ms for simple tool calls
- **P95 Latency**: < 800ms for complex operations
- **P99 Latency**: < 2s for resource-intensive operations
- **Streaming Response Time**: < 100ms per chunk for SSE

### 3.2 Throughput Benchmarks

**Source**: [Performance Benchmarking Research](https://milvus.io/ai-quick-reference/how-do-i-monitor-performance-of-model-context-protocol-mcp-tools-and-resources)

```erlang
% Pattern: Throughput monitoring
-spec record_throughput(Method :: binary(), Count :: pos_integer()) -> ok.

record_throughput(Method, Count) ->
    % Record throughput metrics
    Metrics = #{
        method => Method,
        count => Count,
        timestamp => erlang:system_time(microsecond),
        duration_ms => 1000  % Per second
    },
    erlmcp_metrics:record('throughput_msg_per_s', Count).
```

**Target Throughput:**
- **Core Operations**: 2.69M ops/sec (registry/queue operations)
- **Network I/O**: 43K msg/sec (4KB real packets)
- **Sustained Load**: 372K msg/sec (60M ops/30s)
- **Active Connections**: 40-50K per node

### 3.3 Memory Usage Targets

```erlang
% Pattern: Memory monitoring
-spec record_memory_usage(ConnectionId :: binary()) -> ok.

record_memory_usage(ConnectionId) ->
    % Record memory metrics per connection
    MemoryInfo = erlang:memory([binary, process, total]),
    Metrics = #{
        connection_id => ConnectionId,
        memory_heap => maps:get(process_heap_used, MemoryInfo),
        memory_rss => maps:get(system_total, MemoryInfo),
        timestamp => erlang:system_time(microsecond)
    },
    erlmcp_metrics:record('memory_heap_mib_per_conn',
                         maps:get(process_heap_used, MemoryInfo) / 1024 / 1024 / 1024).
```

**Target Memory Usage:**
- **Per Connection**: < 1MB heap usage
- **RSS per Node**: Monitor for unexpected growth
- **Peak Memory**: Handle 100K+ connections with stable memory

---

## 4. Transport-Specific Patterns

### 4.1 STDIO Transport Patterns

**Source**: [Understanding MCP Through Raw STDIO Communication](https://foojay.io/today/understanding-mcp-through-raw-stdio-communication/)

```erlang
% Pattern: STDIO framing with line-based JSON
-spec handle_stdio_data(Binary :: binary()) -> list(erlmcp:message()).

handle_stdio_data(Binary) ->
    % Split on newlines and parse JSON
    Lines = binary:split(Binary, <<"\n">>, [global]),
    parse_json_lines(Lines, []).

parse_json_lines([<<>>], Acc) -> lists:reverse(Acc);
parse_json_lines([Line|Rest], Acc) ->
    case jsx:is_json(Line) of
        true ->
            Message = jsx:decode(Line, [{labels, atom}]),
            parse_json_lines(Rest, [Message|Acc]);
        false ->
            % Handle malformed JSON
            Error = #{
                jsonrpc => "2.0",
                id => null,
                error => #{
                    code => -32700,  // Parse error
                    message => "Invalid JSON"
                }
            },
            parse_json_lines(Rest, [Error|Acc])
    end.
```

**STDIO Implementation Patterns:**
- **Line-based framing**: Messages separated by newlines
- **JSON validation**: Use jesse for schema validation
- **Error handling**: Graceful handling of malformed messages
- **Backpressure**: Handle slow readers with appropriate buffering

### 4.2 HTTP/SSE Transport Patterns

**Source**: [MCP Transport Protocols](https://medium.com/@SuriNaren/mcp-transport-protocols-adb33970731c)

```erlang
% Pattern: HTTP with SSE streaming
-spec handle_http_request(Req :: cowboy_req:req()) -> cowboy_req:req().

handle_http_request(Req) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            % Handle JSON-RPC request
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Message = jsx:decode(Body, [{labels, atom}]),
            Response = handle_jsonrpc(Message),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>},
                            jsx:encode(Response), Req2);
        <<"GET">> ->
            % Handle SSE stream
            cowboy_req:stream_reply(200, #{
                <<"content-type">> => <<"text/event-stream">>,
                <<"cache-control">> => <<"no-cache">>
            }, Req),
            stream_sse_events(Req)
    end.

stream_sse_events(Req) ->
    % Stream events to client
    Event = format_sse_event(#{
        type => "progress",
        data => jsx:encode(#{progress => 50})
    }),
    cowboy_req:stream(Event, Req),
    timer:sleep(1000),
    stream_sse_events(Req).
```

**HTTP/SSE Patterns:**
- **Dual-endpoint design**: POST for requests, GET/SSE for streaming
- **CORS headers**: Proper origin validation for web clients
- **Chunked encoding**: Use chunked transfer for streaming responses
- **Connection management**: Handle HTTP keep-alive appropriately

### 4.3 WebSocket Transport Patterns (Experimental)

**Source**: [SEP-1288: WebSocket Transport](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1288)

```erlang
% Pattern: WebSocket bidirectional communication
-spec websocket_handle(Frame :: frame(), State :: state()) -> {reply, Reply :: frame(), NewState :: state()}.

websocket_handle({text, JsonFrame}, State) ->
    % Handle WebSocket JSON frame
    Message = jsx:decode(JsonFrame, [{labels, atom}]),
    Response = handle_jsonrpc(Message),
    {reply, {text, jsx:encode(Response)}, State};
websocket_handle({binary, BinaryFrame}, State) ->
    % Handle binary WebSocket frames
    {reply, {binary, BinaryFrame}, State};
websocket_handle(_Frame, State) ->
    {noreply, State}.
```

**WebSocket Patterns:**
- **Bidirectional communication**: Full duplex message exchange
- **Subprotocol negotiation**: Handle MCP-specific WebSocket subprotocols
- **Connection state**: Maintain connection state for persistent sessions
- **Error recovery**: Automatic reconnection with exponential backoff

---

## 5. Error Handling Best Practices

### 5.1 JSON-RPC Error Code Patterns

**Source**: [JSON-RPC Object Specification](https://jsonrpc.org/historical/json-rpc-object-specification.html)

```erlang
% Pattern: Standard error code handling
-spec handle_error(Code :: integer(), Message :: binary(), Data :: map()) -> map().

handle_error(-32600, Message, Data) ->
    % Parse error
    #{
        jsonrpc => "2.0",
        id => null,
        error => #{
            code => -32600,
            message => Message,
            data => Data
        }
    };
handle_error(-32601, Message, Data) ->
    % Method not found
    #{
        jsonrpc => "2.0",
        id => null,
        error => #{
            code => -32601,
            message => Message,
            data => Data#{<<"method_exists">> => check_method_exists(Data)}
        }
    };
handle_error(-32602, Message, Data) ->
    % Invalid params
    #{
        jsonrpc => "2.0",
        id => null,
        error => #{
            code => -32602,
            message => Message,
            data => Data#{<<"validation_errors">> => get_validation_errors(Data)}
        }
    };
handle_error(-32603, Message, Data) ->
    % Internal error
    #{
        jsonrpc => "2.0",
        id => null,
        error => #{
            code => -32603,
            message => Message,
            data => Data#{<<"error_id">> => generate_error_id()}
        }
    }.
```

### 5.2 MCP-Specific Error Handling

```erlang
% Pattern: MCP tool execution errors
-spec handle_tool_error(Tool :: binary(), Error :: term()) -> map().

handle_tool_error(Tool, Error) ->
    case Error of
        {resource_not_found, Path} ->
            #{
                code => 1001,  // MCP-specific error code
                message => <<"Resource not found">>,
                data => #{
                    tool => Tool,
                    path => Path,
                    suggestions => suggest_alternatives(Path)
                }
            };
        {permission_denied, Resource} ->
            #{
                code => 1002,  // MCP-specific error code
                message => <<"Permission denied">>,
                data => #{
                    tool => Tool,
                    resource => Resource,
                    required_permissions => get_required_permissions(Resource)
                }
            };
        {timeout, Operation} ->
            #{
                code => 1003,  // MCP-specific error code
                message => <<"Operation timed out">>,
                data => #{
                    tool => Tool,
                    operation => Operation,
                    timeout_ms => get_timeout_config(Operation)
                }
            }
    end.
```

### 5.3 Error Recovery Patterns

```erlang
% Pattern: Circuit breaker pattern for error recovery
-spec handle_with_circuit_breaker(Func :: function()) -> {ok, Result :: term()} | {error, Reason :: term()}.

handle_with_circuit_breaker(Func) ->
    case erlmcp_circuit_breaker:check_state() of
        {ok, healthy} ->
            try Func() of
                Result -> {ok, Result}
            catch
                Class:Reason ->
                    log_error(Class, Reason),
                    erlmcp_circuit_breaker:trip(),
                    {error, Reason}
            end;
        {error, tripped} ->
            {error, {circuit_breaker, "Service temporarily unavailable"}}
    end.
```

---

## 6. Experimental Feature Implementation Patterns

### 6.1 Streaming Implementation

```erlang
% Pattern: Streaming responses with progress tokens
-spec call_tool_with_stream(Tool :: binary(), Params :: map()) -> {ok, stream()}.

call_tool_with_stream(Tool, Params) ->
    % Create progress token
    ProgressToken = generate_progress_token(),

    % Start streaming process
    StreamPid = spawn(fun() ->
        stream_progress_updates(ProgressToken, Tool),
        execute_tool_streaming(Tool, Params, ProgressToken)
    end),

    {ok, #{
        stream_id => ProgressToken,
        stream_pid => StreamPid
    }}.

stream_progress_updates(ProgressToken, Tool) ->
    % Send progress updates
    Updates = [
        #{progress => 25, status => "Starting"},
        #{progress => 50, status => "Processing"},
        #{progress => 75, status => "Finalizing"}
    ],
    lists:foreach(fun(Update) ->
        send_progress_update(ProgressToken, Tool, Update),
        timer:sleep(1000)
    end, Updates).
```

### 6.2 Experimental Headers

```erlang
% Pattern: Experimental feature detection
-spec handle_experimental_headers(Headers :: map()) -> ok.

handle_experimental_headers(Headers) ->
    case maps:get(<<"x-mcp-experimental">>, Headers, undefined) of
        undefined ->
            ok;
        ExperimentalFeatures ->
            lists:foreach(fun(Feature) ->
                case Feature of
                    <<"streaming">> ->
                        enable_streaming();
                    <<"batching">> ->
                        enable_batching();
                    _ ->
                        log_unknown_feature(Feature)
                end
            end, ExperimentalFeatures)
    end.
```

---

## 7. Implementation Checklist

### 7.1 Testing Checklist

- [ ] **Transport Testing**: Test all transports (stdio, HTTP, WebSocket)
- [ ] **Conformance Testing**: Implement official conformance test suite
- [ ] **Error Handling**: Test all JSON-RPC error codes
- [ ] **Performance**: Validate against baseline targets
- [ ] **Security**: OAuth 2.1 and SEP implementations
- [ ] **Streaming**: Test SSE and streaming capabilities
- [ ] **Integration**: E2E testing with real clients

### 7.2 Security Checklist

- [ ] **OAuth 2.1 Implementation**: Full RFC compliance
- [ ] **Dynamic Client Registration**: RFC 7591 compliance
- [ ] **Resource Metadata**: RFC 9728 compliance
- [ ] **URL Registration**: SEP-991 implementation
- [ ] **Transport Security**: TLS for all transports
- [ ] **Input Validation**: Schema validation for all inputs
- [ ] **Rate Limiting**: Prevent abuse attacks

### 7.3 Performance Checklist

- [ ] **Latency Monitoring**: Track P50/P95/P99 latencies
- [ ] **Throughput Monitoring**: Track msg/sec metrics
- [ ] **Memory Monitoring**: Track per-connection memory usage
- [ ] **Benchmarking**: Regular performance testing
- [ ] **Load Testing**: Test under peak load conditions
- [ ] **Regression Testing**: No performance regressions

---

## 8. References

### 8.1 Official SDKs
- [TypeScript SDK](https://github.com/modelcontextprotocol/typescript-sdk)
- [Python SDK](https://github.com/modelcontextprotocol/python-sdk)
- [Go SDK](https://github.com/modelcontextprotocol/go-sdk)
- [PHP SDK](https://github.com/modelcontextprotocol/php-sdk)
- [C# SDK](https://github.com/modelcontextprotocol/csharp-sdk)

### 8.2 Specifications
- [MCP Conformance Tests](https://github.com/modelcontextprotocol/conformance)
- [JSON-RPC 2.0 Specification](https://jsonrpc.org/specification.html)
- [OAuth 2.0 Protected Resource Metadata](https://datatracker.ietf.org/doc/html/rfc9728)
- [SEP-835](https://mcp-docs.cn/community/seps/985-align-oauth-20-protected-resource-metadata-with-rf)
- [SEP-991](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/991)
- [SEP-1288](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1288)

### 8.3 Community Resources
- [@mcp-testing/server-tester](https://www.npmjs.com/package/@mcp-testing/server-tester)
- [MCP Best Practices](https://modelcontextprotocol.info/docs/best-practices/)
- [Performance Monitoring Guide](https://milvus.io/ai-quick-reference/how-do-i-monitor-performance-of-model-context-protocol-mcp-tools-and-resources)

---

## 9. Conclusion

This patterns catalog provides comprehensive guidance for implementing MCP-compliant systems based on official SDKs and community implementations. Key takeaways:

1. **Test Comprehensively**: Use transport-agnostic testing and official conformance suites
2. **Implement Security**: Follow OAuth 2.1 and SEP specifications strictly
3. **Monitor Performance**: Track latency, throughput, and memory metrics
4. **Handle Errors Gracefully**: Use both JSON-RPC and MCP-specific error codes
5. **Support Transports**: Implement stdio, HTTP/SSE, and experimental WebSocket
6. **Embrace Experimentation**: Support experimental features while maintaining stability

The erlmcp implementation should leverage these patterns to achieve full MCP compliance and maintain high performance, security, and reliability standards.