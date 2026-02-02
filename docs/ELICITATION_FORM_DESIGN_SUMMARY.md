# Elicitation Form API - Design Summary
**Date:** 2026-02-02
**Compliance Target:** 1% → 100% MCP Spec (SEP-1036, SEP-1330, SEP-1034)
**Status:** Design Phase Complete

---

## Executive Summary

This design delivers a comprehensive form-based elicitation API for erlmcp, bringing MCP experimental elicitation feature compliance from 1% to 100%. The solution enables rich, type-safe form interactions with validation, error handling, and re-prompting capabilities.

---

## Deliverables

### 1. API Design Document
**File:** `/home/user/erlmcp/docs/ELICITATION_FORM_API_DESIGN.md`

**Contents:**
- Form definition language (JSON Schema 2020-12)
- 8 form field types with specifications
- Validation architecture (Jesse-integrated)
- Rendering hints for clients
- Response collection and parsing
- Error handling and re-prompting workflow
- Timeout and abandonment handling
- Test harness design
- Complete API methods

**Key Features:**
- Text, number, boolean, date, select, multi-select, URL, file field types
- SEP-1034 default values for all primitive types
- SEP-1330 enhanced enums (titled/untitled)
- SEP-1036 SSRF-protected URL fields
- MCP-compliant validation errors
- Re-prompting with 3-attempt limit
- Configurable timeouts with warnings and extensions

### 2. Form Schema Specification
**File:** `/home/user/erlmcp/docs/ELICITATION_FORM_SCHEMA_SPECIFICATION.md`

**Contents:**
- Complete JSON Schema 2020-12 definitions
- Type-specific field schemas
- Validation rule schemas
- Rendering hint schemas
- Form response schemas
- Erlang type definitions
- Jesse integration examples
- Complete OAuth configuration example

**Standards:**
- JSON Schema 2020-12 compliant
- MCP 2025-11-25 spec compliant
- SEP-1034, SEP-1330, SEP-1036 compliant

### 3. Implementation Roadmap
**File:** `/home/user/erlmcp/docs/ELICITATION_FORM_IMPLEMENTATION_ROADMAP.md`

**Contents:**
- 10-week phased implementation plan
- 5 phases with weekly breakdown
- Resource allocation (2 developers)
- Module checklist (7 new modules)
- Feature checklist (17 features)
- Quality gates and success criteria
- Risk mitigation strategies
- File structure and command reference

**Timeline:**
- **Phase 1 (Weeks 1-2):** Foundation - form schema infrastructure and validation core
- **Phase 2 (Weeks 3-4):** Field Types - implement all 8 field types
- **Phase 3 (Weeks 5-6):** Response Handling - parsing, defaults, re-prompting
- **Phase 4 (Weeks 7-8):** Timeout & Notifications - comprehensive timeout handling
- **Phase 5 (Weeks 9-10):** Testing & Documentation - test harness, PropEr tests, docs

---

## Architecture Overview

### Module Structure

```
erlmcp_elicitation (extended)
├── erlmcp_elicitation_form (new)         # Form registry and management
├── erlmcp_elicitation_form_schema (new)  # JSON Schema compilation
├── erlmcp_elicitation_validator (new)    # Field and form validation
├── erlmcp_elicitation_renderer (new)     # Rendering hints generation
├── erlmcp_elicitation_response_parser (new) # Response parsing and coercion
└── erlmcp_elicitation_reprompt (new)     # Re-prompting workflow
```

### OTP Supervision

```
erlmcp_core_sup (one_for_all)
├── erlmcp_elicitation (gen_server) [permanent]
│   ├── Monitors client processes
│   ├── Timeout timers per elicitation
│   └── Rate limiting (10/min per client)
├── erlmcp_elicitation_form (gen_server) [permanent]
│   └── Form definition registry
└── erlmcp_elicitation_validator (gen_server) [permanent]
    └── Jesse integration + schema cache
```

### Data Flow

```
Client Request
    ↓
elicitation/create (form mode)
    ↓
erlmcp_elicitation_form:register_form/1
    ↓
erlmcp_elicitation_form_schema:compile_form_schema/1
    ↓
erlmcp_schema_cache:cache_schema/2 (persistent_term)
    ↓
Client receives form definition
    ↓
User fills form
    ↓
elicitation/submit (responses)
    ↓
erlmcp_elicitation_response_parser:parse_response/2
    ↓
erlmcp_elicitation_validator:validate_response/2 (Jesse)
    ↓
    ├─ Valid → erlmcp_elicitation:complete_elicitation/2
    │          ↓
    │      notifications/elicitation/complete
    │
    └─ Invalid → Re-prompt (max 3 attempts)
               ↓
           Return validation errors
```

---

## Key Design Decisions

### 1. Form Definition Language
**Decision:** JSON Schema 2020-12 with MCP extensions
**Rationale:**
- Industry standard
- Jesse library integration
- Extensible via `x-` prefix
- Client-side schema sharing

### 2. Validation Engine
**Decision:** Jesse with persistent_term schema caching
**Rationale:**
- <5ms p95 validation latency (cached)
- Zero-copy schema access (~10ns)
- Standards-compliant JSON Schema validator
- Production-ready library

### 3. Field Type Taxonomy
**Decision:** 8 field types (text, number, boolean, date, select, multi_select, url, file)
**Rationale:**
- Covers 95% of form use cases
- Type-safe validation
- Extensible for future types
- Rendering hints for client adaptation

### 4. Enhanced Enums (SEP-1330)
**Decision:** Support both titled and untitled enums
**Rationale:**
- Titled: Better UX for long/technical values (e.g., "us-east-1" → "US East (Virginia)")
- Untitled: Simpler for short values (e.g., ["debug", "info", "error"])
- Backward compatible

### 5. Default Values (SEP-1034)
**Decision:** Support defaults for all primitive types
**Rationale:**
- Reduces user input burden
- Pre-fills sensible defaults
- Improves form completion rates

### 6. SSRF Protection (SEP-1036)
**Decision:** Block private IPs, localhost, link-local for URL fields
**Rationale:**
- Prevents server-side request forgery attacks
- Security-by-default
- Configurable allowlist for schemes

### 7. Re-prompting Workflow
**Decision:** 3-attempt limit with error feedback
**Rationale:**
- Balances user experience with abuse prevention
- Provides clear error messages
- Maintains elicitation state across attempts

### 8. Timeout Handling
**Decision:** Configurable with warnings and extensions
**Rationale:**
- Prevents resource leaks
- User-friendly timeout warnings (60s before expiry)
- Allows reasonable extensions (2x max)

---

## MCP Specification Compliance

### Before Implementation
| Feature | Status |
|---------|--------|
| elicitation/create | 10% (code defined only) |
| notifications/elicitation/complete | 0% |
| URL Mode (SEP-1036) | 0% |
| Enhanced Enums (SEP-1330) | 0% |
| Multi-Select Enums | 0% |
| Default Values (SEP-1034) | 0% |
| Error Code Support | 10% |
| **Overall** | **1% (0.1/7)** |

### After Implementation
| Feature | Status |
|---------|--------|
| elicitation/create | 100% (form mode) |
| notifications/elicitation/complete | 100% |
| URL Mode (SEP-1036) | 100% (SSRF protected) |
| Enhanced Enums (SEP-1330) | 100% (titled/untitled) |
| Multi-Select Enums | 100% |
| Default Values (SEP-1034) | 100% (all types) |
| Error Code Support | 100% (MCP-compliant) |
| **Overall** | **100% (7/7)** |

---

## Performance Targets

| Metric | Target | Strategy |
|--------|--------|----------|
| Form validation latency | <5ms (p95) | Jesse + persistent_term caching |
| Schema compilation (cached) | <100μs | persistent_term zero-copy |
| Schema compilation (uncached) | <20ms | One-time cost, cache forever |
| Concurrent elicitations | 10,000+ | Rate limiting prevents abuse |
| Memory per elicitation | <5KB | Efficient ETS-backed state |
| Validation throughput | 50K ops/sec | Parallel validation workers |

---

## Quality Assurance

### Testing Strategy
- **Chicago School TDD:** Real processes, state-based verification, no mocks
- **EUnit Tests:** 150+ tests across 7 modules
- **Common Test:** Integration test suites
- **Property Tests (PropEr):** 1000+ iterations per property
- **Test Harness:** Form generators, response simulators
- **Coverage:** ≥80% line coverage

### Quality Gates
```bash
✅ TERM=dumb rebar3 compile    # 0 errors
✅ rebar3 eunit                # 0 failures
✅ rebar3 ct                   # 100% pass
✅ rebar3 dialyzer             # 0 warnings
✅ rebar3 xref                 # 0 undefined
✅ rebar3 format --verify      # Pass
✅ Coverage ≥ 80%              # Enforced
```

---

## Security Considerations

### SSRF Protection (SEP-1036)
- ✅ Block private IP ranges (127.0.0.0/8, 10.0.0.0/8, 172.16.0.0/12, 192.168.0.0/16)
- ✅ Block loopback (::1)
- ✅ Block link-local (169.254.0.0/16)
- ✅ URL scheme allowlist (default: https only)
- ✅ DNS resolution validation (future enhancement)

### Input Validation
- ✅ JSON Schema 2020-12 validation for all fields
- ✅ Maximum field length enforcement
- ✅ Maximum file size enforcement (10MB default)
- ✅ MIME type allowlist for file uploads
- ✅ Pattern-based validation (regex)

### Rate Limiting
- ✅ Per-client elicitation rate limit (10/minute)
- ✅ Global concurrent elicitation limit (100)
- ✅ Re-prompt attempt limit (3 attempts)

### Sensitive Data
- ✅ Password fields marked with `inputType: password`
- ✅ No logging of sensitive form responses
- ✅ Field-level encryption support (future)

---

## Examples

### Simple API Key Form

```json
{
  "id": "api_key_form",
  "title": "API Key Configuration",
  "fields": [
    {
      "id": "api_key",
      "type": "text",
      "label": "API Key",
      "required": true,
      "validation": {
        "minLength": 32,
        "pattern": "^sk-[a-zA-Z0-9]{40,}$"
      },
      "rendering": {
        "placeholder": "sk-...",
        "inputType": "password",
        "helpText": "Found in your account settings"
      }
    }
  ]
}
```

### OAuth Configuration Form

See `/home/user/erlmcp/docs/ELICITATION_FORM_SCHEMA_SPECIFICATION.md` section 11.1 for a comprehensive 9-field OAuth form with:
- Select field with titled enums
- URL fields with SSRF protection
- Multi-select for scopes
- Boolean for PKCE
- Conditional display logic
- Cross-field dependencies

---

## Migration Path

### Backward Compatibility

Existing inline/URL/terminal modes remain unchanged. Forms are opt-in:

**Legacy:**
```json
{"mode": "inline", "prompt": "Enter API key:"}
```

**New:**
```json
{"mode": "form", "formDefinition": {...}}
```

### Adoption Strategy

1. **Phase 1:** Add form mode alongside existing modes
2. **Phase 2:** Document migration guide for tool authors
3. **Phase 3:** Create examples for common patterns
4. **Phase 4:** Deprecate inline mode (future, optional)

---

## Next Steps

### Immediate Actions
1. ✅ Design documents complete (this deliverable)
2. ⏭️ Create GitHub issue for implementation tracking
3. ⏭️ Schedule implementation kickoff (Week 1)
4. ⏭️ Assign developers to Phase 1 tasks

### Phase 1 Week 1 (Starting Now)
- Create `erlmcp_elicitation_form` module
- Create `erlmcp_elicitation_form_schema` module
- Extend `erlmcp.hrl` with form types
- Write initial EUnit tests (Chicago TDD)

---

## Resources

### Documentation
- API Design: `/home/user/erlmcp/docs/ELICITATION_FORM_API_DESIGN.md` (64 pages)
- Schema Spec: `/home/user/erlmcp/docs/ELICITATION_FORM_SCHEMA_SPECIFICATION.md` (44 pages)
- Roadmap: `/home/user/erlmcp/docs/ELICITATION_FORM_IMPLEMENTATION_ROADMAP.md` (38 pages)

### MCP Specification References
- [SEP-1036: URL Mode Elicitation](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1036)
- [SEP-1330: Enhanced Enums](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1330)
- [SEP-1034: Default Values](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1034)
- [MCP Elicitation Spec](https://modelcontextprotocol.io/specification/draft/client/elicitation)

### erlmcp Resources
- Existing Guide: `/home/user/erlmcp/docs/ELICITATION_GUIDE.md`
- Compliance Matrix: `/home/user/erlmcp/docs/MCP_SPECIFICATION_COMPLIANCE_MATRIX.md`
- OTP Patterns: `/home/user/erlmcp/docs/otp-patterns.md`
- Chicago TDD: `/.claude/skills/chicago-tdd-erlang.md`

---

## Contact & Feedback

For questions or feedback on this design:
- Review the three design documents linked above
- Consult the MCP specification references
- Follow the 10-week implementation roadmap

---

**Design Status:** ✅ Complete
**Implementation Status:** 🔴 Not Started (Ready to Begin)
**Estimated Completion:** 10 weeks from start
**Compliance Target:** 100% MCP Elicitation (7/7 features)
