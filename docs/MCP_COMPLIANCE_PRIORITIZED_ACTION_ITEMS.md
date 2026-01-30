# MCP 2025-11-25 Compliance - Prioritized Action Items

**Generated:** 2025-01-30
**Based On:** MCP_2025-11-25_COMPLIANCE_GAP_ANALYSIS.md
**Target:** 95%+ compliance with MCP 2025-11-25 specification

---

## PRIORITY CLASSIFICATION

- **P0 - Critical:** Security vulnerabilities, protocol compliance blockers
- **P1 - High:** Major feature gaps affecting interoperability
- **P2 - Medium:** Feature enhancements, standards compliance
- **P3 - Low:** Documentation, nice-to-have features

---

## PHASE 1: CRITICAL COMPLIANCE (Weeks 1-3)

### P0-1: Tasks API Implementation
**Status:** ❌ NOT IMPLEMENTED
**Effort:** 5 days
**Dependencies:** None
**Blocker:** Long-running workflow support

**Action Items:**
1. Design task manager architecture (gen_server)
2. Implement task CRUD operations:
   - `tasks/create` - Create async task
   - `tasks/list` - List all tasks
   - `tasks/get` - Get task status
   - `tasks/result` - Retrieve task result
   - `tasks/cancel` - Cancel task
3. Add task status notification support
4. Implement task persistence (ETS)
5. Add expiration and cleanup
6. Write comprehensive tests (unit + integration)
7. Update documentation

**Acceptance Criteria:**
- [ ] All 5 task methods implemented and tested
- [ ] Task status notifications working
- [ ] Task persistence across restarts
- [ ] 80%+ test coverage
- [ ] Documentation updated

**Files to Create:**
- `/src/erlmcp_task_manager.erl`
- `/src/erlmcp_task_sup.erl`
- `/test/erlmcp_task_manager_tests.erl`
- `/test/erlmcp_task_integration_tests.erl`

**Files to Modify:**
- `/include/erlmcp.hrl` - Add task records and constants
- `/src/erlmcp_server.erl` - Integrate task creation
- `/src/erlmcp_capabilities.erl` - Add tasks capability

---

### P0-2: HTTP Origin Header Validation
**Status:** ⚠️ PARTIALLY IMPLEMENTED
**Effort:** 1 day
**Dependencies:** None
**Blocker:** Security vulnerability

**Action Items:**
1. Audit HTTP transport implementation
2. Add Origin header extraction
3. Implement origin validation logic
4. Return HTTP 403 for invalid origins
5. Add configuration for allowed origins
6. Write security tests

**Acceptance Criteria:**
- [ ] Origin header validated on all requests
- [ ] HTTP 403 returned for invalid origins
- [ ] Configurable allowed origins list
- [ ] Security tests passing

**Files to Modify:**
- `/src/erlmcp_transport_http.erl` (or equivalent)
- `/test/erlmcp_http_transport_security_tests.erl`

---

### P0-3: OAuth 2.0 OpenID Connect Discovery
**Status:** ⚠️ PARTIALLY IMPLEMENTED
**Effort:** 2 days
**Dependencies:** None
**Blocker:** OAuth 2.0 compliance

**Action Items:**
1. Implement OpenID Connect discovery client
2. Fetch `/.well-known/openid-configuration`
3. Parse and cache discovery document
4. Extract authorization/token endpoints
5. Handle discovery failures gracefully

**Acceptance Criteria:**
- [ ] Automatic endpoint discovery working
- [ ] Discovery document cached appropriately
- [ ] Graceful fallback on discovery failure
- [ ] Integration tests with real IdP

**Files to Create:**
- `/src/erlmcp_auth_oidc.erl`

**Files to Modify:**
- `/src/erlmcp_auth.erl`
- `/test/erlmcp_auth_oidc_tests.erl`

---

### P0-4: Incremental Scope Consent (WWW-Authenticate)
**Status:** ❌ NOT IMPLEMENTED
**Effort:** 2 days
**Dependencies:** P0-3
**Blocker:** OAuth 2.0 compliance

**Action Items:**
1. Implement WWW-Authenticate header parsing
2. Support incremental scope requests
3. Handle scope consent responses
4. Update auth flow to support incremental scopes

**Acceptance Criteria:**
- [ ] WWW-Authenticate header parsed correctly
- [ ] Incremental scope requests working
- [ ] Scope consent responses handled
- [ ] Integration tests passing

**Files to Modify:**
- `/src/erlmcp_auth.erl`
- `/src/erlmcp_auth_oauth.erl`

---

### P0-5: OAuth Client Metadata Validation
**Status:** ❌ NOT IMPLEMENTED
**Effort:** 1 day
**Dependencies:** None
**Blocker:** OAuth 2.0 compliance

**Action Items:**
1. Implement client metadata document fetch
2. Validate client metadata structure
3. Support client registration endpoint
4. Add metadata caching

**Acceptance Criteria:**
- [ ] Client metadata fetched and validated
- [ ] Client registration supported
- [ ] Metadata cached appropriately
- [ ] Validation tests passing

**Files to Create:**
- `/src/erlmcp_auth_client_metadata.erl`

---

### P0-6: Input Validation Error Fix
**Status:** ⚠️ PARTIALLY COMPLIANT
**Effort:** 1 day
**Dependencies:** None
**Blocker:** Model self-correction support

**Action Items:**
1. Audit all input validation paths
2. Change protocol errors to tool execution errors
3. Update error codes:
   - `?JSONRPC_INVALID_PARAMS` → `?MCP_ERROR_INVALID_TOOL_ARGUMENTS`
   - Add validation details to error data
4. Write validation error tests

**Acceptance Criteria:**
- [ ] All validation errors use tool error codes
- [ ] Validation details in error data
- [ ] Model self-correction enabled
- [ ] Tests passing

**Files to Modify:**
- `/src/erlmcp_server.erl` - Tool execution paths
- `/src/erlmcp_tool.erl` - Tool validation
- `/test/erlmcp_validation_error_tests.erl`

---

### P0-7: Compliance Test Suite Framework
**Status:** ❌ NOT IMPLEMENTED
**Effort:** 3 days
**Dependencies:** None
**Blocker:** Quality assurance

**Action Items:**
1. Design compliance test framework
2. Implement core protocol tests
3. Implement capability negotiation tests
4. Implement error code validation tests
5. Add test reporting and badges

**Acceptance Criteria:**
- [ ] Compliance test framework functional
- [ ] Core protocol tests passing
- [ ] Capability tests passing
- [ ] Error code tests passing
- [ ] Compliance report generated

**Files to Create:**
- `/test/erlmcp_compliance_suite.erl`
- `/test/erlmcp_compliance_core_tests.erl`
- `/test/erlmcp_compliance_capabilities_tests.erl`
- `/test/erlmcp_compliance_errors_tests.erl`
- `/test/erlmcp_compliance_reporter.erl`

---

## PHASE 2: HIGH PRIORITY FEATURES (Weeks 4-5)

### P1-1: SSE Polling Support
**Status:** ❌ NOT IMPLEMENTED
**Effort:** 3 days
**Dependencies:** None
**Blocker:** HTTP transport reliability

**Action Items:**
1. Implement server-initiated SSE disconnect
2. Add GET stream polling support
3. Implement stream resumption (via GET)
4. Encode stream identity in event IDs
5. Add stream expiration and cleanup

**Acceptance Criteria:**
- [ ] Server can disconnect SSE streams
- [ ] Clients can poll via GET
- [ ] Streams resumable from last event ID
- [ ] Stream identity encoded in events
- [ ] Integration tests passing

**Files to Modify:**
- `/src/erlmcp_sse_event_store.erl`
- `/src/erlmcp_transport_http.erl`
- `/test/erlmcp_sse_polling_tests.erl`

---

### P1-2: Completion API Implementation
**Status:** ❌ NOT IMPLEMENTED
**Effort:** 3 days
**Dependencies:** None
**Blocker:** IDE integration feature

**Action Items:**
1. Design completion handler interface
2. Implement `completion/complete` method
3. Add completion capability negotiation
4. Implement context-aware completion
5. Add completion to sampling integration

**Acceptance Criteria:**
- [ ] Completion API functional
- [ ] Capability negotiation working
- [ ] Context-aware completion supported
- [ ] Integration with sampling
- [ ] Tests passing

**Files to Create:**
- `/src/erlmcp_completion.erl`
- `/test/erlmcp_completion_tests.erl`

**Files to Modify:**
- `/include/erlmcp.hrl` - Add completion records
- `/src/erlmcp_server.erl` - Add completion handler
- `/src/erlmcp_capabilities.erl` - Add completion capability

---

### P1-3: Elicitation API Implementation
**Status:** ⚠️ ERROR CODES ONLY
**Effort:** 3 days
**Dependencies:** None
**Blocker:** User interaction feature

**Action Items:**
1. Implement `elicitation/create` method
2. Implement `notifications/elicitation/complete`
3. Add URL mode support (SEP-1036)
4. Implement enhanced enum schemas (SEP-1330)
5. Add default values support (SEP-1034)

**Acceptance Criteria:**
- [ ] Elicitation API functional
- [ ] URL mode working
- [ ] Enhanced enums (titled/untitled, single/multi-select)
- [ ] Default values for all primitives
- [ ] Tests passing

**Files to Create:**
- `/src/erlmcp_elicitation.erl`
- `/test/erlmcp_elicitation_tests.erl`

**Files to Modify:**
- `/include/erlmcp.hrl` - Add elicitation records
- `/src/erlmcp_server.erl` - Add elicitation handler
- `/src/erlmcp_capabilities.erl` - Add elicitation capability

---

## PHASE 3: MEDIUM PRIORITY ENHANCEMENTS (Weeks 6-7)

### P2-1: Icons Support
**Status:** ⚠️ CACHE EXISTS
**Effort:** 2 days
**Dependencies:** None
**Blocker:** UI enhancement

**Action Items:**
1. Add icon field to tool metadata
2. Add icon field to resource metadata
3. Add icon field to prompt metadata
4. Implement icon URL validation
5. Enhance icon fetching and caching
6. Add icon tests

**Acceptance Criteria:**
- [ ] Icons in tool metadata
- [ ] Icons in resource metadata
- [ ] Icons in prompt metadata
- [ ] Icon URL validation working
- [ ] Icon cache functional
- [ ] Tests passing

**Files to Modify:**
- `/include/erlmcp.hrl` - Add icon fields to records
- `/src/erlmcp_icon_cache.erl` - Enhance icon handling
- `/src/erlmcp_server.erl` - Support icon metadata
- `/test/erlmcp_icon_tests.erl`

---

### P2-2: JSON Schema 2020-12 Compliance
**Status:** ⚠️ USING JESSE
**Effort:** 2 days
**Dependencies:** None
**Blocker:** Standards compliance

**Action Items:**
1. Verify jesse library supports 2020-12
2. Set 2020-12 as default dialect
3. Add schema version detection
4. Update validation error messages
5. Write dialect tests

**Acceptance Criteria:**
- [ ] JSON Schema 2020-12 default
- [ ] Schema version detection working
- [ ] Error messages match 2020-12 spec
- [ ] Tests passing

**Files to Modify:**
- `/src/erlmcp_schema_registry.erl`
- `/test/erlmcp_schema_2020_12_tests.erl`

---

### P2-3: Sampling Enhancement
**Status:** ⚠️ BASIC IMPLEMENTATION
**Effort:** 2 days
**Dependencies:** P1-2 (Completion)
**Blocker:** Feature parity

**Action Items:**
1. Add tool calling support to sampling
2. Implement `tools` parameter
3. Implement `toolChoice` parameter
4. Add `includeContext` parameter
5. Write sampling tests

**Acceptance Criteria:**
- [ ] Tool calling in sampling working
- [ ] toolChoice parameter supported
- [ ] includeContext parameter supported
- [ ] Tests passing

**Files to Modify:**
- `/src/erlmcp_sampling.erl`
- `/src/erlmcp_mock_llm.erl`
- `/test/erlmcp_sampling_enhanced_tests.erl`

---

### P2-4: Roots Enhancement
**Status:** ⚠️ BASIC IMPLEMENTATION
**Effort:** 1 day
**Dependencies:** None
**Blocker:** Feature parity

**Action Items:**
1. Implement `roots/list_changed` notification
2. Add root list tracking
3. Send notifications on root changes
4. Write roots tests

**Acceptance Criteria:**
- [ ] roots/list_changed notification working
- [ ] Root list tracked
- [ ] Notifications sent on changes
- [ ] Tests passing

**Files to Modify:**
- `/src/erlmcp_server.erl`
- `/src/erlmcp_client.erl`
- `/test/erlmcp_roots_tests.erl`

---

## PHASE 4: DOCUMENTATION & CLEANUP (Week 8)

### P3-1: SDK Tier Classification
**Status:** ❌ NOT DOCUMENTED
**Effort:** 1 day
**Dependencies:** All features complete
**Blocker:** Documentation

**Action Items:**
1. Define erlmcp SDK tier
2. Create feature support matrix
3. Document maintenance commitments
4. Publish tier classification

**Acceptance Criteria:**
- [ ] SDK tier defined
- [ ] Feature matrix created
- [ ] Maintenance commitments documented
- [ ] Published in README

**Files to Create:**
- `/docs/SDK_TIER_CLASSIFICATION.md`

---

### P3-2: Security Best Practices Documentation
**Status:** ⚠️ NEEDS UPDATE
**Effort:** 1 day
**Dependencies:** P0 items complete
**Blocker:** Documentation

**Action Items:**
1. Update security documentation
2. Document OAuth 2.0 flows
3. Add tool description security guidance
4. Document user consent requirements

**Acceptance Criteria:**
- [ ] Security docs updated
- [ ] OAuth flows documented
- [ ] Tool security guidance added
- [ ] Consent requirements documented

**Files to Modify:**
- `/docs/SECURITY.md`
- `/docs/AUTHENTICATION.md`

---

### P3-3: API Documentation Update
**Status:** ⚠️ NEEDS UPDATE
**Effort:** 2 days
**Dependencies:** All features complete
**Blocker:** Documentation

**Action Items:**
1. Update API reference
2. Document new capabilities
3. Add code examples
4. Update migration guide

**Acceptance Criteria:**
- [ ] API reference complete
- [ ] New capabilities documented
- [ ] Code examples added
- [ ] Migration guide updated

**Files to Modify:**
- `/docs/api-reference.md`
- `/docs/migration-guide.md`

---

### P3-4: Deprecated Feature Audit
**Status:** ⚠️ NEEDS AUDIT
**Effort:** 2 days
**Dependencies:** All features complete
**Blocker:** Code quality

**Action Items:**
1. Audit code for deprecated features
2. Mark deprecated APIs
3. Remove or update deprecated code
4. Update deprecation warnings

**Acceptance Criteria:**
- [ ] Deprecated features identified
- [ ] APIs marked as deprecated
- [ ] Unnecessary code removed
- [ ] Warnings updated

---

## SUMMARY

### Total Effort Estimate
- **Phase 1 (P0):** 15 days (3 weeks)
- **Phase 2 (P1):** 9 days (2 weeks)
- **Phase 3 (P2):** 7 days (1.5 weeks)
- **Phase 4 (P3):** 6 days (1.5 weeks)
- **Total:** 37 days (~8 weeks)

### Quick Wins (1-2 days each)
- P0-2: HTTP Origin Validation
- P0-5: OAuth Client Metadata
- P0-6: Input Validation Error Fix
- P2-4: Roots Enhancement
- P3-1: SDK Tier Classification
- P3-2: Security Documentation

### Large Efforts (3+ days each)
- P0-1: Tasks API (5 days)
- P0-7: Compliance Test Suite (3 days)
- P1-1: SSE Polling (3 days)
- P1-2: Completion API (3 days)
- P1-3: Elicitation API (3 days)

### Dependencies
- P0-4 depends on P0-3 (OAuth discovery before incremental scopes)
- P2-3 depends on P1-2 (Completion before sampling enhancement)
- Phase 4 depends on all previous phases (docs after implementation)

### Risk Mitigation
1. **Start with P0-2** (HTTP Origin) - Quick security win
2. **Parallelize P0-1** (Tasks) and **P0-3/P0-4** (OAuth) - Different teams
3. **Implement P0-7 early** (Test suite) - Enable continuous validation
4. **Document as you go** - Reduce Phase 4 burden

---

**Next Steps:**
1. Review and prioritize this list with stakeholders
2. Assign developers to each action item
3. Create project timeline with milestones
4. Set up continuous compliance monitoring
5. Begin Phase 1 implementation

**Contact:** For questions or clarifications, refer to the full gap analysis document at `/docs/MCP_2025-11-25_COMPLIANCE_GAP_ANALYSIS.md`
