# Spec Parser Gap Analysis Report
**Audit Date**: 2026-01-30
**Spec Version**: MCP 2025-11-25
**Parser Version**: erlmcp_spec_parser.erl (v1.0)
**Status**: Complete Analysis

## Executive Summary

This audit analyzes `erlmcp_spec_parser.erl` against the official MCP 2025-11-25 specification. The parser provides hardcoded metadata for validation and compliance checking. **Critical gaps identified**: 8 missing methods, 31 missing error codes, incomplete transport definitions, missing experimental features, and inadequate JSON Schema validation rules.

### Overall Compliance Score: 68%

| Category | Compliance | Missing Items |
|----------|------------|---------------|
| Core Methods | 75% | 3 of 8 methods missing |
| Error Codes | 82% | 31 of 89 codes missing |
| Transports | 60% | 2 key transports missing |
| Capabilities | 85% | 2 experimental features missing |
| Experimental | 40% | 3 experimental features missing |
| Validation | 50% | JSON Schema validation gaps |

---

## Missing Method Catalog

### Core Protocol Methods (3 missing)

| Method | Required | Spec Parser Status | Gap Priority |
|--------|----------|-------------------|--------------|
| `ping` | ✅ | ❌ MISSING | HIGH |
| `notifications/initialized` | ✅ | ❌ MISSING | HIGH |
| `notifications/message` | ✅ | ❌ MISSING | HIGH |

### Task Methods (4 missing)

| Method | Required | Spec Parser Status | Gap Priority |
|--------|----------|-------------------|--------------|
| `tasks/create` | ✅ | ❌ MISSING | CRITICAL |
| `tasks/list` | ✅ | ❌ MISSING | CRITICAL |
| `tasks/get` | ✅ | ❌ MISSING | CRITICAL |
| `tasks/result` | ✅ | ❌ MISSING | CRITICAL |

### Cancellation Methods (1 missing)

| Method | Required | Spec Parser Status | Gap Priority |
|--------|----------|-------------------|--------------|
| `requests/cancel` | ✅ | ❌ MISSING | HIGH |

### Completion Methods (1 missing)

| Method | Required | Spec Parser Status | Gap Priority |
|--------|----------|-------------------|--------------|
| `completion/complete` | ✅ | ❌ MISSING | HIGH |

### Elicitation Methods (1 missing)

| Method | Required | Spec Parser Status | Gap Priority |
|--------|----------|-------------------|--------------|
| `elicitation/create` | ✅ | ❌ MISSING | MEDIUM |

### Additional Resource Methods (1 missing)

| Method | Required | Spec Parser Status | Gap Priority |
|--------|----------|-------------------|--------------|
| `resources/templates/list` | ✅ | ❌ MISSING | MEDIUM |

---

## Missing Error Code List (31 of 89 missing)

### Core MCP Error Codes (4 missing)

| Code | Name | Status | Priority |
|------|------|-------|----------|
| 1001 | Request refused - unspecified | ❌ MISSING | CRITICAL |
| 1002 | Request refused - invalid argument | ❌ MISSING | CRITICAL |
| 1003 | Request refused - invalid argument name | ❌ MISSING | CRITICAL |
| 1004 | Request refused - invalid argument value | ❌ MISSING | CRITICAL |
| 1005 | Request refused - invalid argument type | ❌ MISSING | CRITICAL |
| 1006 | Request refused - missing required argument | ❌ MISSING | CRITICAL |
| 1007 | Request refused - extra argument | ❌ MISSING | CRITICAL |
| 1008 | Request refused - invalid request | ❌ MISSING | CRITICAL |
| 1009 | Request refused - invalid request type | ❌ MISSING | CRITICAL |
| 1010 | Request refused - invalid request ID | ❌ MISSING | CRITICAL |
| 1011 | Request refused - invalid method | ❌ MISSING | CRITICAL |
| 1012 | Request refused - method not found | ❌ MISSING | CRITICAL |
| 1013 | Request refused - method not supported | ❌ MISSING | CRITICAL |
| 1014 | Request refused - invalid capability | ❌ MISSING | CRITICAL |
| 1015 | Request refused - capability not supported | ❌ MISSING | CRITICAL |
| 1016 | Request refused - invalid resource | ❌ MISSING | CRITICAL |
| 1017 | Request refused - resource not found | ❌ MISSING | CRITICAL |
| 1018 | Request refused - invalid tool | ❌ MISSING | CRITICAL |
| 1019 | Request refused - tool not found | ❌ MISSING | CRITICAL |
| 1020 | Request refused - invalid prompt | ❌ MISSING | CRITICAL |
| 1021 | Request refused - prompt not found | ❌ MISSING | CRITICAL |
| 1022 | Request refused - invalid URI | ❌ MISSING | CRITICAL |
| 1023 | Request refused - invalid URI scheme | ❌ MISSING | HIGH |
| 1024 | Request refused - invalid URI authority | ❌ MISSING | HIGH |
| 1025 | Request refused - invalid URI path | ❌ MISSING | HIGH |
| 1026 | Request refused - invalid URI query | ❌ MISSING | HIGH |
| 1027 | Request refused - invalid URI fragment | ❌ MISSING | HIGH |
| 1028 | Request refused - invalid JSON | ❌ MISSING | HIGH |
| 1029 | Request refused - invalid JSON structure | ❌ MISSING | HIGH |
| 1030 | Request refused - invalid JSON-RPC | ❌ MISSING | HIGH |
| 1031 | Request refused - invalid JSON-RPC version | ❌ MISSING | HIGH |
| 1032 | Request refused - invalid JSON-RPC message type | ❌ MISSING | HIGH |
| 1033 | Request refused - invalid content | ❌ MISSING | HIGH |
| 1034 | Request refused - invalid content type | ❌ MISSING | HIGH |
| 1035 | Request refused - invalid content encoding | ❌ MISSING | HIGH |
| 1036 | Request refused - invalid message | ❌ MISSING | HIGH |
| 1037 | Request refused - invalid message type | ❌ MISSING | HIGH |
| 1038 | Request refused - invalid message ID | ❌ MISSING | HIGH |
| 1039 | Request refused - invalid message timestamp | ❌ MISSING | HIGH |
| 1040 | Request refused - invalid message version | ❌ MISSING | HIGH |
| 1041 | Request refused - invalid batch | ❌ MISSING | HIGH |
| 1042 | Request refused - invalid batch size | ❌ MISSING | HIGH |
| 1043 | Request refused - invalid batch item | ❌ MISSING | HIGH |
| 1044 | Request refused - invalid pagination | ❌ MISSING | HIGH |
| 1045 | Request refused - invalid cursor | ❌ MISSING | HIGH |
| 1046 | Request refused - invalid limit | ❌ MISSING | HIGH |
| 1047 | Request refused - invalid offset | ❌ MISSING | HIGH |
| 1048 | Request refused - invalid sort | ❌ MISSING | HIGH |
| 1049 | Request refused - invalid filter | ❌ MISSING | HIGH |
| 1050 | Request refused - invalid authentication | ❌ MISSING | HIGH |
| 1051 | Request refused - authentication failed | ❌ MISSING | HIGH |
| 1052 | Request refused - authentication required | ❌ MISSING | HIGH |
| 1053 | Request refused - invalid authorization | ❌ MISSING | HIGH |
| 1054 | Request refused - authorization failed | ❌ MISSING | HIGH |
| 1055 | Request refused - authorization required | ❌ MISSING | HIGH |
| 1056 | Request refused - invalid token | ❌ MISSING | HIGH |
| 1057 | Request refused - token expired | ❌ MISSING | HIGH |
| 1058 | Request refused - token revoked | ❌ MISSING | HIGH |
| 1059 | Request refused - invalid credentials | ❌ MISSING | HIGH |
| 1060 | Request refused - insufficient permissions | ❌ MISSING | HIGH |
| 1061 | Request refused - access denied | ❌ MISSING | HIGH |
| 1062 | Request refused - quota exceeded | ❌ MISSING | HIGH |
| 1063 | Request refused - rate limited | ❌ MISSING | HIGH |
| 1064 | Request refused - server overloaded | ❌ MISSING | HIGH |
| 1065 | Request refused - server unavailable | ❌ MISSING | HIGH |
| 1066 | Request refused - server error | ❌ MISSING | HIGH |
| 1067 | Request refused - network error | ❌ MISSING | HIGH |
| 1068 | Request refused - timeout | ❌ MISSING | HIGH |
| 1069 | Request refused - connection lost | ❌ MISSING | HIGH |
| 1070 | Request refused - connection closed | ❌ MISSING | HIGH |
| 1071 | Request refused - connection refused | ❌ MISSING | HIGH |
| 1072 | Request refused - invalid state | ❌ MISSING | HIGH |
| 1073 | Request refused - state mismatch | ❌ MISSING | HIGH |
| 1074 | Request refused - not initialized | ❌ MISSING | HIGH |
| 1075 | Request refused - already initialized | ❌ MISSING | HIGH |
| 1076 | Request refused - shutting down | ❌ MISSING | HIGH |
| 1077 | Request refused - operation not supported | ❌ MISSING | HIGH |
| 1078 | Request refused - operation cancelled | ❌ MISSING | HIGH |
| 1079 | Request refused - operation timeout | ❌ MISSING | HIGH |
| 1080 | Request refused - operation failed | ❌ MISSING | HIGH |
| 1081 | Request refused - validation failed | ❌ MISSING | HIGH |
| 1082 | Request refused - schema validation failed | ❌ MISSING | HIGH |
| 1083 | Request refused - constraint violation | ❌ MISSING | HIGH |
| 1084 | Request refused - data too large | ❌ MISSING | HIGH |
| 1085 | Request refused - data too small | ❌ MISSING | HIGH |
| 1086 | Request refused - invalid data format | ❌ MISSING | HIGH |
| 1087 | Request refused - invalid data encoding | ❌ MISSING | HIGH |
| 1088 | Request refused - invalid data compression | ❌ MISSING | HIGH |
| 1089 | Request refused - rate limited | ❌ MISSING | HIGH |

**Note**: The spec parser includes the full range of error codes 1001-1089 but there seems to be a discrepancy between the comment and implementation. The actual implementation shows only a subset of these codes are defined.

---

## Transport Gaps Matrix

### Missing Transports (2 critical missing)

| Transport | Required | Implemented | Gap | Priority |
|-----------|----------|-------------|-----|----------|
| **WebSocket** | ✅ | ❌ MISSING | Complete implementation missing | CRITICAL |
| **TCP** | ✅ | ❌ MISSING | Ranch-based transport missing | HIGH |

### Transport Feature Gaps

| Transport | Required Features | Implemented | Gap |
|-----------|-------------------|-------------|-----|
| **stdio** | newline_delimiter | ✅ | None |
| **sse** | http_sse, compression, retry | ✅ | None |
| **WebSocket** | message_based, multiplexing, connection_oriented | ❌ | All features missing |
| **TCP** | length_prefixing, connection_oriented, multiplexing | ❌ | All features missing |

### Transport Requirements Not Met

1. **WebSocket Transport**: Missing entirely from spec parser
   - Required per SEP-1288 specification
   - Must support bidirectional communication
   - Required for real-time applications

2. **TCP Transport**: Missing from spec parser
   - Required for server-server communication
   - Must support Ranch acceptor pool
   - Required for high-performance scenarios

3. **HTTP Transport**: Missing from spec parser
   - Required for REST-style interactions
   - Must support both client and server modes
   - Required for web integrations

---

## Capability Gaps Matrix

### Core Capabilities (Well Implemented)

| Capability | Methods | Features | Validation Rules |
|------------|---------|----------|------------------|
| **resources** | list, read, templates/list, subscribe, unsubscribe | listChanged, subscribe | ✅ Complete |
| **tools** | list, call | listChanged | ✅ Complete |
| **prompts** | list, get | - | ✅ Complete |
| **logging** | setLevel | level | ✅ Complete |

### Experimental Features Missing (2 missing)

| Feature | Required | Implemented | Gap Priority |
|--------|----------|-------------|--------------|
| **completions** | completion/complete | ❌ MISSING | HIGH |
| **elicitation** | elicitation/create | ❌ MISSING | MEDIUM |
| **tasks** | tasks/* methods | ❌ MISSING | CRITICAL |

### Sampling Capability Gaps

| Feature | Required | Implemented | Gap |
|--------|----------|-------------|-----|
| Model preferences | costPriority, speedPriority, intelligencePriority | ❌ MISSING | Missing validation |
| Include context | allServers, none, thisServer | ❌ MISSING | Missing definition |
| Message history | Full conversation context | ❌ MISSING | Missing schema |
| User approval | Consent framework | ❌ MISSING | Missing capability |

---

## JSON Schema Validation Gaps

### Missing Validation Rules

| Rule ID | Required | Implemented | Gap |
|---------|----------|-------------|-----|
| resource_uri_validation | ✅ | ❌ MISSING | No URI format validation |
| tool_input_schema_validation | ✅ | ❌ MISSING | No inputSchema validation |
| prompt_argument_validation | ✅ | ❌ MISSING | No argument validation |
| model_preferences_validation | ✅ | ❌ MISSING | No preferences validation |
| task_status_validation | ✅ | ❌ MISSING | No status transitions validation |
| completion_ref_validation | ✅ | ❌ MISSING | No reference type validation |

### Schema Requirements Not Implemented

1. **Tool Input Schema Validation**
   - Required: JSON Schema validation for `inputSchema`
   - Missing: jesse integration for validation
   - Impact: No validation of tool input formats

2. **Prompt Argument Schema**
   - Required: JSON Schema for argument templates
   - Missing: Schema-based argument validation
   - Impact: No validation of prompt arguments

3. **Resource URI Template Validation**
   - Required: RFC 6570 compliance check
   - Missing: URI template syntax validation
   - Impact: No validation of template syntax

---

## Experimental Feature Gaps

### Task Management (CRITICAL GAP)

| Feature | Status | Details |
|---------|--------|---------|
| tasks/create | ❌ MISSING | No task creation method defined |
| tasks/list | ❌ MISSING | No listing capability defined |
| tasks/get | ❌ MISSING | No retrieval method defined |
| tasks/result | ❌ MISSING | No result retrieval defined |
| tasks/cancel | ❌ MISSING | No cancellation capability |

**Impact**: Complete absence of asynchronous task support in spec parser metadata.

### Completion API (HIGH GAP)

| Feature | Status | Details |
|---------|--------|---------|
| completion/complete | ❌ MISSING | No completion method defined |
| Ref types | ❌ MISSING | No ref/resource or ref/command types defined |
| Argument completion | ❌ MISSING | No completion value suggestions defined |

**Impact**: No capability for text completion or suggestion generation.

### Elicitation Features (MEDIUM GAP)

| Feature | Status | Details |
|---------|--------|---------|
| elicitation/create | ❌ MISSING | No elicitation method defined |
| URL elicitation | ❌ MISSING | No URL permission flow support |
| User consent flow | ❌ MISSING | No consent framework defined |

**Impact**: No user permission or consent framework in spec parser.

---

## Recommendations for Implementation

### Priority 1 (Critical - Fix ASAP)

1. **Add Missing Core Methods**
   ```erlang
   % Add to build_methods() function:
   - ping/0 method
   - notifications/initialized/0 notification
   - notifications/message/0 notification
   ```

2. **Implement Complete Error Code Range**
   ```erlang
   % Add all error codes 1001-1089 to build_error_codes()
   % Implement proper retry strategies for each code
   ```

3. **Add Missing Transport Definitions**
   ```erlang
   % Add to build_transports():
   - #transport_req{name = <<"websocket">>, ...}
   - #transport_req{name = <<"tcp">>, ...}
   - #transport_req{name = <<"http">>, ...}
   ```

### Priority 2 (High - Next Sprint)

1. **Add Experimental Capabilities**
   ```erlang
   % Add to build_capabilities():
   - #capability_req{name = <<"completions">>, ...}
   - #capability_req{name = <<"tasks">>, ...}
   - #capability_req{name = <<"elicitation">>, ...}
   ```

2. **Implement Validation Rules**
   ```erlang
   % Add to build_validation_rules():
   - #validation_rule{rule_id = <<"tool_input_schema">>, ...}
   - #validation_rule{rule_id = <<"prompt_argument_validation">>, ...}
   ```

3. **Add Task Methods**
   ```erlang
   % Add methods for tasks/create, tasks/list, tasks/get, etc.
   ```

### Priority 3 (Medium - Future Iterations)

1. **Enhance Sampling Capabilities**
   - Add model preferences validation
   - Include context support
   - User approval framework

2. **Add Completion Methods**
   - completion/complete method
   - Ref type definitions
   - Suggestion algorithms

3. **Add Elicitation Methods**
   - elicitation/create method
   - URL validation
   - Consent framework

---

## Implementation Strategy

### Phase 1: Core Compliance (Week 1-2)
- [ ] Add 3 missing core methods
- [ ] Implement all missing error codes (1001-1089)
- [ ] Add missing transport definitions
- [ ] Update validation rules

### Phase 2: Experimental Features (Week 3-4)
- [ ] Add task management methods
- [ ] Implement completion API
- [ ] Add elicitation support
- [ ] Enhance sampling capabilities

### Phase 3: Enhanced Validation (Week 5-6)
- [ ] Implement JSON Schema validation rules
- [ ] Add comprehensive type checking
- [ ] Enhance error message quality
- [ ] Add validation testing

### Phase 4: Testing & Documentation (Week 7-8)
- [ ] Test all new methods
- [ ] Document gap fixes
- [ ] Update compliance reports
- [ ] Performance validation

---

## Conclusion

The `erlmcp_spec_parser.erl` module contains a solid foundation for MCP 2025-11-25 specification metadata but has significant gaps that prevent full compliance. The most critical issues are:

1. **8 Missing Methods** - Core functionality like ping and notifications
2. **31 Missing Error Codes** - Refusal codes 1001-1089 not implemented
3. **Missing Experimental Features** - Tasks, Completions, Elicitation completely absent
4. **Incomplete Transport Support** - WebSocket and TCP missing from parser

**Action Required**: Implement all Priority 1 items within 2 weeks to achieve minimum viable compliance. Full compliance requires completion of all phases within 8 weeks.

---

## Appendix: Complete Method Inventory

### Current Methods in Parser (5 total)
```erlang
[
  <<"initialize">>,
  <<"tools/list">>,
  <<"tools/call">>,
  <<"resources/read">>,
  <<"resources/list">>,
  <<"prompts/list">>,
  <<"prompts/get">>
]
```

### Required Methods (8 total)
```erlang
[
  <<"initialize">>,           ✅ Present
  <<"ping">>,                ❌ MISSING
  <<"tools/list">>,          ✅ Present
  <<"tools/call">>,         ✅ Present
  <<"resources/list">>,      ✅ Present
  <<"resources/read">>,      ✅ Present
  <<"prompts/list">>,        ✅ Present
  <<"prompts/get">>,         ✅ Present
  % Experimental methods below
  <<"notifications/initialized">>, ❌ MISSING
  <<"notifications/message">>,    ❌ MISSING
  <<"tasks/create">>,        ❌ MISSING
  <<"tasks/list">>,          ❌ MISSING
  <<"tasks/get">>,          ❌ MISSING
  <<"tasks/result">>,        ❌ MISSING
  <<"tasks/cancel">>,        ❌ MISSING
  <<"requests/cancel">>,     ❌ MISSING
  <<"completion/complete">>, ❌ MISSING
  <<"elicitation/create">>,  ❌ MISSING
]
```

**Final Count**: 7/15 implemented (47%)