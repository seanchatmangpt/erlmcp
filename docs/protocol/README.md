# MCP Protocol Documentation

Complete technical specifications and guides for the Model Context Protocol (MCP) and JSON-RPC 2.0 implementation in erlmcp v2.2.0.

---

## Quick Navigation

### For Architects & Protocol Engineers
Start with **[PROTOCOL_LAYER_ANALYSIS.md](PROTOCOL_LAYER_ANALYSIS.md)**
- System architecture and message flow
- Performance characteristics
- Security analysis
- Compliance matrix

### For Integration & API Users
Start with **[MESSAGE_FORMAT_GUIDE.md](MESSAGE_FORMAT_GUIDE.md)**
- Real-world JSON examples
- Erlang code snippets
- Error scenarios
- Best practices

### For Specification & Reference
Start with **[MCP_JSON_RPC_SPECIFICATION.md](MCP_JSON_RPC_SPECIFICATION.md)**
- Complete message format specification
- Error codes taxonomy (115+ codes)
- Batch request processing
- Message size limits

### For Protocol Handshake
Start with **[initialization.md](initialization.md)**
- Initialize request/response flow
- State machine verification
- Request ID safety
- Protocol version negotiation

---

## Document Outline

### 1. PROTOCOL_LAYER_ANALYSIS.md
**Comprehensive technical analysis** (~2,500 words)

#### Sections:
- **1. Protocol Architecture** - Layering model, module breakdown
- **2. Message Type Taxonomy** - Message classification tree, lifecycle
- **3. Request Lifecycle** - Complete client request flow diagram
- **4. Response & Error Handling** - Response format, error classification
- **5. Notification System** - Types, triggers, implementation
- **6. Batch Request Processing** - Rules, error handling, algorithm
- **7. Protocol Initialization** - Handshake, state machine
- **8. Message Size Limits** - Gap #45 implementation
- **9. Request ID Safety** - Overflow protection, collision detection
- **10. Performance Analysis** - Benchmarks, optimization techniques
- **11. Compliance & Testing** - Test coverage matrix
- **12. Integration Points** - Client-server-transport interaction
- **13. Key Constants** - Complete definitions from erlmcp.hrl
- **14. Troubleshooting Guide** - Common errors, detection
- **15. Future Enhancements** - Proposed improvements
- **16. References** - File paths, external docs

---

### 2. MCP_JSON_RPC_SPECIFICATION.md
**Detailed technical specification** (~3,000 words)

#### Sections:
- **1. JSON-RPC 2.0 Message Structure** - Base format, field definitions
- **2. Request Messages** - Structure, IDs, safety, encoding/decoding
- **3. Response Messages** - Success/error format, encoding, rules
- **4. Notification Messages** - Structure, characteristics, examples
- **5. Batch Requests** - Structure, rules, processing, error handling
- **6. Error Codes & Handling** - Standard codes, MCP ranges, experimental
- **7. Message Size Limits** - Validation, transport limits, errors
- **8. Protocol Initialization & Versioning** - Init flow, version handling
- **9. Message Encoding & Serialization** - JSON encoding, binary data
- **10. Message Flow Examples** - Request-response, notifications, subscriptions
- **11. Message Record Types** - Erlang record definitions
- **12. Message Validation** - Validation pipeline, rules
- **13. Performance Characteristics** - Throughput, memory, latency
- **14. Compliance Checklist** - Features covered
- **15. References** - Implementation files, specs

---

### 3. MESSAGE_FORMAT_GUIDE.md
**Practical implementation guide** (~2,000 words)

#### Sections:
- **1. Initialization Handshake** - Request/response examples, Erlang code
- **2. Resource Management** - List, read, subscribe, notify examples
- **3. Tool Management** - Register, list, call, error examples
- **4. Prompt Management** - Register, list, get examples
- **5. Error Scenarios** - 8 common error cases with JSON/Erlang
- **6. Batch Requests** - Batch examples with mixed results
- **7. Content Type Examples** - Text, markdown, JSON, binary, image, PDF
- **8. Annotations & Resource Links** - Examples with metadata
- **9. Sampling & LLM Preferences** - Model preference handling
- **10. Completion/Elicitation Examples** - Completion API
- **11. Progress Tracking** - Progress token notifications
- **12. Best Practices** - Request ID management, error handling, timeouts
- **13. Encoding/Decoding Examples** - Complete code examples

---

### 4. initialization.md
**Protocol initialization & safety** (~800 words)

#### Sections:
- **State Machine Diagrams** - Server and client states
- **Protocol Safety Rules (P0)** - Initialize must be first/only once
- **Request ID Safety** - Overflow detection, collision detection, bounds
- **Error Codes** - Spec-compliant error codes
- **Testing & Validation** - Test suite coverage
- **Tracing & Observability** - OTEL integration, logging
- **Security Implications** - Attack vectors mitigated
- **Compliance Checklist** - All requirements verified

---

## Key Statistics

### Message Counts
- **Record Types**: 3 (request, response, notification)
- **Error Code Ranges**: 11 major ranges
- **Total Error Codes**: 115+ distinct codes
- **Standard JSON-RPC Codes**: 5 codes
- **MCP-Specific Codes**: 110+ codes
- **Experimental Codes**: 10 codes (1090-1099)

### Performance Baselines
- **Request Encoding**: 2.69M ops/sec
- **Request Decoding**: 2.69M ops/sec
- **Notification Encoding**: 2.69M ops/sec
- **Batch Processing (10 msg)**: 269K ops/sec
- **Latency (p99)**: < 1 microsecond (single message), < 5 microseconds (batch)

### Size Limits by Transport
| Transport | Default Max | Rationale |
|-----------|-------------|-----------|
| stdio | 10 MB | Process memory |
| tcp | 100 MB | Socket buffer |
| http | 50 MB | HTTP server |
| websocket | 50 MB | WS implementation |
| sse | 10 MB | Streaming size |
| default | unlimited | No enforcement |

### Test Coverage
- **Unit Tests**: 40+ tests in `erlmcp_json_rpc_tests.erl`
- **Categories**: Request/response/notification/batch/error/decode
- **Compliance**: JSON-RPC 2.0 and MCP 2025-11-25
- **Coverage Target**: ≥80% code coverage

---

## Implementation Files

### Core Protocol
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl` (1089 lines)
  - Main protocol encoder/decoder
  - Error code definitions
  - Error helper functions (100+)

- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_message_parser.erl` (126 lines)
  - Optimized hot-path parsing
  - Message type detection
  - Parameter validation

- `/home/user/erlmcp/apps/erlmcp_core/include/erlmcp.hrl` (1124 lines)
  - All constants and definitions
  - Record definitions
  - Error codes

### Client/Server
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_client.erl`
  - Client-side message handling
  - Request correlation
  - Async response handling

- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`
  - Server-side message dispatch
  - Method registration
  - Capability negotiation

### Tests
- `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl` (400+ lines)
  - 40+ unit tests
  - Request/response/notification/batch/error scenarios
  - Chicago School TDD (no mocks)

- `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_message_parser_tests.erl`
  - Message parser tests
  - Fast-path optimization verification

---

## Common Tasks

### "How do I encode a request?"
→ See **MESSAGE_FORMAT_GUIDE.md § 13.1** + **MCP_JSON_RPC_SPECIFICATION.md § 2.4**

### "How do I handle errors?"
→ See **PROTOCOL_LAYER_ANALYSIS.md § 4** + **MESSAGE_FORMAT_GUIDE.md § 5**

### "What are the error codes?"
→ See **MCP_JSON_RPC_SPECIFICATION.md § 6** for complete taxonomy

### "How does batch processing work?"
→ See **PROTOCOL_LAYER_ANALYSIS.md § 6** + **MESSAGE_FORMAT_GUIDE.md § 6**

### "How is initialization enforced?"
→ See **initialization.md** + **PROTOCOL_LAYER_ANALYSIS.md § 7**

### "What are the performance characteristics?"
→ See **PROTOCOL_LAYER_ANALYSIS.md § 10** + **MCP_JSON_RPC_SPECIFICATION.md § 13**

### "How do I validate message sizes?"
→ See **MCP_JSON_RPC_SPECIFICATION.md § 7** + **PROTOCOL_LAYER_ANALYSIS.md § 8**

### "What's the protocol version?"
→ See **MCP_JSON_RPC_SPECIFICATION.md § 8** (version `2025-11-25`)

---

## Protocol Versions

### Current Version
- **MCP Protocol**: `2025-11-25`
- **JSON-RPC**: `2.0`
- **erlmcp**: v2.2.0

### Version History
- **2025-11-25**: Current (MCP spec alignment)
- **2024-11-05**: Previous (legacy)

---

## Error Code Quick Reference

### Standard JSON-RPC 2.0 (-32700 to -32603)
| Code | Name | Meaning |
|------|------|---------|
| -32700 | Parse error | Invalid JSON |
| -32600 | Invalid Request | Malformed structure |
| -32601 | Method not found | Unknown method |
| -32602 | Invalid params | Wrong parameters |
| -32603 | Internal error | Server exception |

### Core MCP (-32001 to -32010)
| Code | Name | Meaning |
|------|------|---------|
| -32001 | Resource not found | Unknown resource URI |
| -32002 | Tool not found | Unknown tool name |
| -32003 | Prompt not found | Unknown prompt name |
| -32004 | Capability not supported | Unsupported feature |
| -32005 | Not initialized | RPC before initialize |

### Full Taxonomy
See **MCP_JSON_RPC_SPECIFICATION.md § 6** for complete error code ranges:
- Content errors: -32011 to -32020
- Resource errors: -32021 to -32030
- Tool errors: -32031 to -32040
- Prompt errors: -32041 to -32050
- Auth errors: -32051 to -32060
- Protocol errors: -32061 to -32070
- Pagination errors: -32071 to -32080
- Task errors: -32081 to -32090
- Progress errors: -32091 to -32100
- Completion errors: -32110 to -32113
- Experimental errors: 1090 to 1099

---

## Compliance & Standards

### JSON-RPC 2.0 Compliance
- ✓ Request/response/notification structure
- ✓ Error code range (-32000 to -32699)
- ✓ Batch request support
- ✓ Request ID correlation

### MCP 2025-11-25 Compliance
- ✓ Initialize handshake
- ✓ Protocol version negotiation
- ✓ Capability exchange
- ✓ Resource/tool/prompt management
- ✓ Notification system
- ✓ Error handling

### Security Features
- ✓ Request ID overflow detection (2^60 limit)
- ✓ Request ID collision detection
- ✓ Message size validation (Gap #45)
- ✓ Pre-initialization RPC blocking
- ✓ Double-initialize rejection
- ✓ Input validation

---

## Getting Started

### 1. Understand the Protocol
```bash
# Read the complete specification
less MCP_JSON_RPC_SPECIFICATION.md

# Understand the architecture
less PROTOCOL_LAYER_ANALYSIS.md
```

### 2. See Examples
```bash
# View practical examples
less MESSAGE_FORMAT_GUIDE.md

# Check initialization flow
less initialization.md
```

### 3. Review Implementation
```bash
# Core module
less /home/user/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl

# Tests
less /home/user/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl
```

### 4. Run Tests
```bash
cd /home/user/erlmcp
rebar3 eunit --module=erlmcp_json_rpc_tests
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_protocol_init_SUITE.erl
```

---

## Contributing to Protocol Documentation

### Update Checklist
- [ ] Edit markdown file
- [ ] Update cross-references
- [ ] Verify code examples compile
- [ ] Run tests: `rebar3 eunit`
- [ ] Commit with reference to docs

---

## Related Documentation

- **Session Persistence**: `/home/user/erlmcp/docs/SESSION_PERSISTENCE.md`
- **Secrets Management**: `/home/user/erlmcp/docs/SECRETS_MANAGEMENT.md`
- **OTP Patterns**: `/home/user/erlmcp/docs/otp-patterns.md`
- **Architecture**: `/home/user/erlmcp/docs/architecture.md`

---

## Support & References

### External Resources
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
- [MCP Specification](https://github.com/modelcontextprotocol/specification)

### Internal Resources
- Implementation: `/home/user/erlmcp/apps/erlmcp_core/`
- Tests: `/home/user/erlmcp/apps/erlmcp_core/test/`
- Examples: `/home/user/erlmcp/examples/`

---

## Document Maintenance

**Last Updated**: January 31, 2026
**Version**: erlmcp v2.2.0
**Maintainer**: erlmcp project team

---

## Summary

This protocol documentation provides:

1. **Complete Specification** (MCP_JSON_RPC_SPECIFICATION.md)
   - All message formats, error codes, validation rules

2. **Technical Analysis** (PROTOCOL_LAYER_ANALYSIS.md)
   - Architecture, performance, compliance, security

3. **Practical Guide** (MESSAGE_FORMAT_GUIDE.md)
   - Real examples, code snippets, best practices

4. **Initialization Details** (initialization.md)
   - Handshake flow, state machine, safety guarantees

**Total Coverage**: 7,500+ words of technical documentation
**Audience**: Architects, engineers, integration specialists, API users
**Purpose**: Complete understanding of MCP protocol in erlmcp v2.2.0
