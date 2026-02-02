# Elicitation Form API - Implementation Roadmap
**Version:** 1.0.0
**Date:** 2026-02-02
**Target Completion:** 10 weeks
**Compliance Goal:** 1% → 100%

---

## Executive Summary

This roadmap outlines a 10-week phased implementation to deliver a production-ready Elicitation Form API for erlmcp, achieving 100% MCP spec compliance for experimental elicitation features (SEP-1036, SEP-1330, SEP-1034).

### Success Criteria

| Metric | Current | Target | Status |
|--------|---------|--------|--------|
| **MCP Compliance** | 1% (0.1/7) | 100% (7/7) | 🔴 |
| **Test Coverage** | 0% | ≥80% | 🔴 |
| **Performance** | N/A | <5ms p95 validation | 🔴 |
| **Documentation** | Partial | Complete | 🟡 |

### Deliverables

1. **8 Form Field Types**: text, number, boolean, date, select, multi_select, url, file
2. **Validation Engine**: Jesse-integrated with schema caching
3. **Re-prompting Workflow**: Multi-attempt validation with error feedback
4. **Timeout Handling**: Configurable timeouts, warnings, and extensions
5. **Test Harness**: Chicago School TDD with 80%+ coverage
6. **Complete Documentation**: API docs, guides, and examples

---

## Phase 1: Foundation (Weeks 1-2)

### Objectives
- Establish form schema infrastructure
- Implement validation engine core
- Achieve compilation and basic tests

### Week 1: Schema Infrastructure

#### Tasks
1. **Create erlmcp_elicitation_form module**
   - `start_link/0` - gen_server initialization
   - `register_form/1` - Register form definition
   - `get_form/1` - Retrieve form by ID
   - `compile_form_schema/1` - Compile to JSON Schema

2. **Create erlmcp_elicitation_form_schema module**
   - `compile_form_schema/1` - Form → JSON Schema
   - `validate_form_definition/1` - Meta-schema validation
   - `field_to_json_schema/1` - Field → JSON Schema property

3. **Extend erlmcp.hrl**
   - Add form-specific error codes
   - Add form type definitions
   - Add field type definitions

#### Deliverables
```erlang
%% New modules
apps/erlmcp_core/src/erlmcp_elicitation_form.erl
apps/erlmcp_core/src/erlmcp_elicitation_form_schema.erl

%% Updated headers
include/erlmcp.hrl (form error codes)

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_form_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_form_schema_tests.erl
```

#### Quality Gates
- ✅ `TERM=dumb rebar3 compile` - 0 errors
- ✅ `rebar3 eunit --module=erlmcp_elicitation_form_tests` - 0 failures
- ✅ `rebar3 dialyzer` - 0 type errors
- ✅ Coverage ≥ 80% for new modules

### Week 2: Validation Engine Core

#### Tasks
1. **Create erlmcp_elicitation_validator module**
   - `start_link/0` - gen_server for validation
   - `validate_response/2` - Validate form response
   - `validate_field/3` - Validate single field
   - Jesse integration with schema cache

2. **Implement basic field types (3)**
   - Text field validation
   - Number field validation
   - Boolean field validation

3. **Error formatting**
   - `format_validation_error/1` - MCP-compliant error format
   - Error code mapping (MCP_ERROR_VALIDATION_FAILED)

#### Deliverables
```erlang
%% New modules
apps/erlmcp_core/src/erlmcp_elicitation_validator.erl

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_validator_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_validation_integration_tests.erl
```

#### Quality Gates
- ✅ Compile + Dialyzer + Xref clean
- ✅ EUnit tests: 100% pass, coverage ≥ 80%
- ✅ Validation latency: <5ms p95 (cached schemas)
- ✅ Jesse schema compilation: <20ms p95

---

## Phase 2: Field Types (Weeks 3-4)

### Objectives
- Implement all 8 form field types
- Add rendering hint support
- Achieve comprehensive field validation

### Week 3: Date, Select, Multi-Select

#### Tasks
1. **Date field**
   - ISO 8601 parsing and validation
   - Date range validation (minimum, maximum)
   - Format validation (date, date-time)

2. **Select field (SEP-1330)**
   - Untitled enum support
   - Titled enum support (oneOf with const/title)
   - Enum value validation

3. **Multi-select field (SEP-1330)**
   - Array of enum values
   - minItems/maxItems validation
   - uniqueItems enforcement

#### Deliverables
```erlang
%% Extended modules
apps/erlmcp_core/src/erlmcp_elicitation_validator.erl
  - validate_date_field/2
  - validate_select_field/2
  - validate_multi_select_field/2

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_date_field_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_select_field_tests.erl (SEP-1330 coverage)
apps/erlmcp_core/test/erlmcp_elicitation_multi_select_field_tests.erl
```

#### Quality Gates
- ✅ All field types compile and pass tests
- ✅ SEP-1330 compliance: titled/untitled enums
- ✅ Coverage ≥ 80% for all field validators

### Week 4: URL, File Fields + Rendering Hints

#### Tasks
1. **URL field (SEP-1036)**
   - SSRF protection integration
   - Scheme allowlist validation
   - Private IP blocking
   - Localhost blocking

2. **File field**
   - File upload metadata validation
   - MIME type validation
   - File size validation
   - Extension validation

3. **Rendering hints module**
   - Create `erlmcp_elicitation_renderer`
   - Generate rendering hints from field definitions
   - Conditional display logic

#### Deliverables
```erlang
%% New modules
apps/erlmcp_core/src/erlmcp_elicitation_renderer.erl

%% Extended modules
apps/erlmcp_core/src/erlmcp_elicitation_validator.erl
  - validate_url_field/2 (SSRF checks)
  - validate_file_field/2

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_url_field_tests.erl (SEP-1036 coverage)
apps/erlmcp_core/test/erlmcp_elicitation_file_field_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_renderer_tests.erl
```

#### Quality Gates
- ✅ SEP-1036 compliance: URL SSRF protection
- ✅ All 8 field types implemented and tested
- ✅ Rendering hints functional
- ✅ Coverage ≥ 80%

---

## Phase 3: Response Handling (Weeks 5-6)

### Objectives
- Implement response parsing and validation
- Build re-prompting workflow
- Add default value support (SEP-1034)

### Week 5: Response Parser + Default Values

#### Tasks
1. **Create erlmcp_elicitation_response_parser**
   - `parse_response/2` - Parse raw response
   - `parse_field/2` - Parse single field value
   - Type coercion (string → number, etc.)

2. **Default values (SEP-1034)**
   - Default value application for missing fields
   - Default value validation against field type
   - Support for all primitive types

3. **Extend erlmcp_elicitation**
   - Add `submit_form_response/2` API
   - Integrate response parser
   - Integrate validator

#### Deliverables
```erlang
%% New modules
apps/erlmcp_core/src/erlmcp_elicitation_response_parser.erl

%% Extended modules
apps/erlmcp_core/src/erlmcp_elicitation.erl
  - submit_form_response/2

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_response_parser_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_default_values_tests.erl (SEP-1034 coverage)
```

#### Quality Gates
- ✅ SEP-1034 compliance: default values for all types
- ✅ Response parsing: 100% field type coverage
- ✅ Type coercion tests pass
- ✅ Coverage ≥ 80%

### Week 6: Re-prompting Workflow

#### Tasks
1. **Create erlmcp_elicitation_reprompt**
   - `handle_submission/3` - Process submission with retries
   - `should_allow_retry/2` - Retry policy
   - `collect_validation_errors/1` - Error aggregation

2. **Multi-attempt validation**
   - Track attempt count per elicitation
   - Configurable max attempts (default: 3)
   - Preserve previous errors for client

3. **Error response formatting**
   - MCP-compliant JSON-RPC error response
   - Include retry metadata (retry_allowed, retry_count, retry_limit)
   - Field-level error details

#### Deliverables
```erlang
%% New modules
apps/erlmcp_core/src/erlmcp_elicitation_reprompt.erl

%% Extended modules
apps/erlmcp_core/src/erlmcp_elicitation.erl
  - Add attempt tracking to state
  - Integrate reprompt logic

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_reprompt_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_reprompt_integration_tests.erl
```

#### Quality Gates
- ✅ Re-prompting workflow functional
- ✅ Max retry enforcement
- ✅ Error response format compliance
- ✅ Coverage ≥ 80%

---

## Phase 4: Timeout & Notifications (Weeks 7-8)

### Objectives
- Implement comprehensive timeout handling
- Add timeout warnings and extensions
- Implement elicitation completion notifications

### Week 7: Timeout Handling

#### Tasks
1. **Extend timeout configuration**
   - Add `timeout_config` to elicitation state
   - Support timeout warnings (notify before expiry)
   - Support timeout extensions

2. **Timeout warning notifications**
   - Send `notifications/elicitation/timeout_warning`
   - Include remaining time and extension availability
   - Configurable warning threshold (default: 60s before timeout)

3. **Timeout extension API**
   - Add `elicitation/extend_timeout` method
   - Track extension count and enforce limits
   - Update timeout_at timestamp

4. **Abandonment detection**
   - Detect client disconnection
   - Detect form invalidation
   - Clean up abandoned elicitations

#### Deliverables
```erlang
%% Extended modules
apps/erlmcp_core/src/erlmcp_elicitation.erl
  - extend_timeout/2
  - send_timeout_warning/1
  - handle_abandonment/2

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_timeout_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_abandonment_tests.erl
```

#### Quality Gates
- ✅ Timeout warnings sent correctly
- ✅ Timeout extensions functional
- ✅ Abandonment cleanup verified
- ✅ Coverage ≥ 80%

### Week 8: Completion Notifications

#### Tasks
1. **Implement notifications/elicitation/complete**
   - Send notification on successful completion
   - Include elicitation ID and result
   - Send to registered client

2. **Integration with erlmcp_server**
   - Add notification sending to server
   - Register notification handler
   - Test end-to-end notification flow

3. **Notification error handling**
   - Handle client disconnection during notification
   - Retry logic for failed notifications
   - Logging and metrics

#### Deliverables
```erlang
%% Extended modules
apps/erlmcp_core/src/erlmcp_elicitation.erl
  - send_completion_notification/2

apps/erlmcp_core/src/erlmcp_server.erl
  - Add elicitation notification support

%% Tests
apps/erlmcp_core/test/erlmcp_elicitation_notification_tests.erl
apps/erlmcp_core/test/erlmcp_elicitation_integration_SUITE.erl (CT)
```

#### Quality Gates
- ✅ Completion notifications sent correctly
- ✅ Server integration functional
- ✅ End-to-end CT tests pass
- ✅ Coverage ≥ 80%

---

## Phase 5: Testing & Documentation (Weeks 9-10)

### Objectives
- Build comprehensive test harness
- Write property tests
- Complete documentation and examples

### Week 9: Test Harness + Property Tests

#### Tasks
1. **Create test harness**
   - `erlmcp_elicitation_form_test_harness` module
   - Form generators (all field types)
   - Valid/invalid response generators
   - User interaction simulator

2. **Property tests (PropEr)**
   - Text field property tests (length, pattern)
   - Number field property tests (range, multipleOf)
   - Select/multi-select enum tests
   - URL SSRF protection tests

3. **Performance benchmarking**
   - Validation latency benchmarks
   - Schema compilation benchmarks
   - Response parsing benchmarks
   - Generate performance report

#### Deliverables
```erlang
%% New modules
apps/erlmcp_core/test/erlmcp_elicitation_form_test_harness.erl
apps/erlmcp_core/test/erlmcp_elicitation_proper_tests.erl

%% Performance tests
apps/erlmcp_core/test/erlmcp_elicitation_performance_SUITE.erl

%% Benchmarks
scripts/benchmark_elicitation_validation.escript
```

#### Quality Gates
- ✅ Test harness generates all field types
- ✅ PropEr tests discover no failures (1000+ iterations)
- ✅ Performance benchmarks meet targets (<5ms p95)
- ✅ Overall test coverage ≥ 80%

### Week 10: Documentation + Examples

#### Tasks
1. **Complete API documentation**
   - Update `docs/ELICITATION_GUIDE.md` with form API
   - Document all form field types
   - Document validation rules
   - Document error handling

2. **Create examples**
   - `examples/elicitation_forms/api_key_form.erl`
   - `examples/elicitation_forms/oauth_config_form.erl`
   - `examples/elicitation_forms/file_upload_form.erl`
   - `examples/elicitation_forms/conditional_form.erl`

3. **Integration guides**
   - Tool integration guide (how tools use forms)
   - Client implementation guide
   - Migration guide (inline → form mode)

4. **Final compliance verification**
   - Run full MCP compliance test suite
   - Verify 100% compliance for elicitation
   - Generate compliance report

#### Deliverables
```markdown
## Documentation
docs/ELICITATION_GUIDE.md (updated)
docs/ELICITATION_FORM_API_DESIGN.md (this doc)
docs/ELICITATION_FORM_SCHEMA_SPECIFICATION.md
docs/ELICITATION_FORM_CLIENT_GUIDE.md (new)
docs/ELICITATION_FORM_MIGRATION_GUIDE.md (new)

## Examples
examples/elicitation_forms/api_key_form.erl
examples/elicitation_forms/oauth_config_form.erl
examples/elicitation_forms/file_upload_form.erl
examples/elicitation_forms/conditional_form.erl
examples/elicitation_forms/ssrf_protected_url_form.erl

## Reports
test_results/elicitation_form_compliance_report.md
test_results/elicitation_form_performance_report.md
```

#### Quality Gates
- ✅ All documentation complete and reviewed
- ✅ All examples compile and run
- ✅ MCP compliance: 100% (7/7 features)
- ✅ make check passes (all quality gates)

---

## Implementation Checklist

### Module Checklist

| Module | Week | Status | Tests | Coverage |
|--------|------|--------|-------|----------|
| erlmcp_elicitation_form | 1 | 🔴 | 0/20 | 0% |
| erlmcp_elicitation_form_schema | 1 | 🔴 | 0/15 | 0% |
| erlmcp_elicitation_validator | 2 | 🔴 | 0/40 | 0% |
| erlmcp_elicitation_renderer | 4 | 🔴 | 0/10 | 0% |
| erlmcp_elicitation_response_parser | 5 | 🔴 | 0/25 | 0% |
| erlmcp_elicitation_reprompt | 6 | 🔴 | 0/15 | 0% |
| erlmcp_elicitation (extended) | 7-8 | 🔴 | +20 | 0% |

### Feature Checklist (MCP Compliance)

| Feature | SEP | Week | Status |
|---------|-----|------|--------|
| Form definition language | SEP-1330 | 1 | 🔴 |
| Text field | SEP-1034 | 2 | 🔴 |
| Number field | SEP-1034 | 2 | 🔴 |
| Boolean field | SEP-1034 | 2 | 🔴 |
| Date field | SEP-1034 | 3 | 🔴 |
| Select (untitled enum) | SEP-1330 | 3 | 🔴 |
| Select (titled enum) | SEP-1330 | 3 | 🔴 |
| Multi-select | SEP-1330 | 3 | 🔴 |
| URL field (SSRF) | SEP-1036 | 4 | 🔴 |
| File field | - | 4 | 🔴 |
| Default values (all types) | SEP-1034 | 5 | 🔴 |
| Response parsing | - | 5 | 🔴 |
| Re-prompting | - | 6 | 🔴 |
| Timeout handling | - | 7 | 🔴 |
| Timeout warnings | - | 7 | 🔴 |
| Timeout extensions | - | 7 | 🔴 |
| notifications/elicitation/complete | MCP | 8 | 🔴 |

---

## Resource Allocation

### Team Structure

**Recommended Team:** 2 developers (can be done with 1 at slower pace)

| Role | Responsibility | Weeks |
|------|---------------|-------|
| **OTP Developer** | Core implementation (modules, OTP patterns) | 1-8 |
| **Test Engineer** | Tests, harness, property tests | 2-9 |
| **Tech Writer** | Documentation and examples | 9-10 |
| **Reviewer** | Code review, quality gates | 1-10 |

### Time Budget

| Phase | Effort (person-weeks) | Calendar Weeks |
|-------|---------------------|----------------|
| Phase 1: Foundation | 2 | 2 |
| Phase 2: Field Types | 2 | 2 |
| Phase 3: Response Handling | 2 | 2 |
| Phase 4: Timeout & Notifications | 2 | 2 |
| Phase 5: Testing & Documentation | 2 | 2 |
| **Total** | **10** | **10** |

---

## Risk Mitigation

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Jesse performance bottleneck | Medium | High | Schema caching with persistent_term (done) |
| Complex conditional validation | Medium | Medium | Start with simple conditions, iterate |
| SSRF bypass | Low | High | Comprehensive security tests, external audit |
| Timeout race conditions | Medium | Medium | Chicago TDD with real processes |

### Schedule Risks

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Underestimated complexity | Low | Medium | 20% buffer in estimates |
| Dependency on jesse library | Low | High | Use stable jesse version, test thoroughly |
| Testing takes longer | Medium | Low | Parallel testing with development |

---

## Quality Assurance

### Continuous Quality Gates

**Every Commit:**
```bash
TERM=dumb rebar3 compile    # Must pass
rebar3 dialyzer             # 0 warnings
rebar3 xref                 # 0 undefined functions
rebar3 format --verify      # Code formatted
```

**Every Module:**
```bash
rebar3 eunit --module=MODULE_tests    # 100% pass
rebar3 cover --module=MODULE          # ≥80% coverage
```

**Every Phase (Weeks 2, 4, 6, 8, 10):**
```bash
make check                  # Full quality check
make test-changed           # Incremental tests
```

**Final Release (Week 10):**
```bash
make quality-report         # Generate compliance report
make benchmark-quick        # Performance verification
```

---

## Success Metrics

### Phase Completion Criteria

Each phase must meet ALL criteria before proceeding:

1. ✅ **Compilation**: `TERM=dumb rebar3 compile` - 0 errors
2. ✅ **Tests**: All EUnit/CT tests pass
3. ✅ **Coverage**: ≥80% line coverage for new code
4. ✅ **Type Safety**: `rebar3 dialyzer` - 0 warnings
5. ✅ **No Undefined Calls**: `rebar3 xref` - 0 undefined
6. ✅ **Formatting**: `rebar3 format --verify` - pass
7. ✅ **Documentation**: Module docs updated
8. ✅ **Review**: Code review approved

### Final Success Criteria (Week 10)

| Criterion | Target | Status |
|-----------|--------|--------|
| **MCP Compliance** | 100% (7/7 features) | 🔴 |
| **Test Coverage** | ≥80% overall | 🔴 |
| **Performance** | <5ms p95 validation | 🔴 |
| **Documentation** | 100% complete | 🔴 |
| **Examples** | 5+ working examples | 🔴 |
| **Quality Gates** | All passing | 🔴 |

---

## Maintenance & Support

### Post-Implementation

1. **Monitoring** (Week 11+)
   - Add metrics to erlmcp_metrics
   - Add OpenTelemetry spans
   - Dashboard integration

2. **Optimization** (Week 12+)
   - Profile validation performance
   - Optimize hot paths
   - Cache optimization

3. **Extension** (Week 13+)
   - Additional field types (color, range, etc.)
   - Advanced conditional validation
   - Custom validators

---

## References

### MCP Specification
- [SEP-1036: URL Mode Elicitation](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1036)
- [SEP-1330: Enhanced Enums](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1330)
- [SEP-1034: Default Values](https://github.com/modelcontextprotocol/modelcontextprotocol/issues/1034)

### erlmcp Resources
- API Design: `/home/user/erlmcp/docs/ELICITATION_FORM_API_DESIGN.md`
- Schema Spec: `/home/user/erlmcp/docs/ELICITATION_FORM_SCHEMA_SPECIFICATION.md`
- Existing Guide: `/home/user/erlmcp/docs/ELICITATION_GUIDE.md`
- Compliance Matrix: `/home/user/erlmcp/docs/MCP_SPECIFICATION_COMPLIANCE_MATRIX.md`

### Implementation Patterns
- OTP Patterns: `/home/user/erlmcp/docs/otp-patterns.md`
- Chicago TDD: `/.claude/skills/chicago-tdd-erlang.md`
- SPARC Workflow: `/home/user/erlmcp/docs/sparc_specification.md`

---

## Appendix A: File Structure

```
apps/erlmcp_core/
├── src/
│   ├── erlmcp_elicitation.erl (extended)
│   ├── erlmcp_elicitation_form.erl (new)
│   ├── erlmcp_elicitation_form_schema.erl (new)
│   ├── erlmcp_elicitation_validator.erl (new)
│   ├── erlmcp_elicitation_renderer.erl (new)
│   ├── erlmcp_elicitation_response_parser.erl (new)
│   └── erlmcp_elicitation_reprompt.erl (new)
│
├── test/
│   ├── erlmcp_elicitation_tests.erl (extended)
│   ├── erlmcp_elicitation_form_tests.erl (new)
│   ├── erlmcp_elicitation_form_schema_tests.erl (new)
│   ├── erlmcp_elicitation_validator_tests.erl (new)
│   ├── erlmcp_elicitation_renderer_tests.erl (new)
│   ├── erlmcp_elicitation_response_parser_tests.erl (new)
│   ├── erlmcp_elicitation_reprompt_tests.erl (new)
│   ├── erlmcp_elicitation_form_test_harness.erl (new)
│   ├── erlmcp_elicitation_proper_tests.erl (new)
│   ├── erlmcp_elicitation_integration_SUITE.erl (new CT)
│   └── erlmcp_elicitation_performance_SUITE.erl (new CT)
│
docs/
├── ELICITATION_FORM_API_DESIGN.md (created)
├── ELICITATION_FORM_SCHEMA_SPECIFICATION.md (created)
├── ELICITATION_FORM_IMPLEMENTATION_ROADMAP.md (this file)
├── ELICITATION_FORM_CLIENT_GUIDE.md (new)
├── ELICITATION_FORM_MIGRATION_GUIDE.md (new)
└── ELICITATION_GUIDE.md (updated)

examples/elicitation_forms/
├── api_key_form.erl (new)
├── oauth_config_form.erl (new)
├── file_upload_form.erl (new)
├── conditional_form.erl (new)
└── ssrf_protected_url_form.erl (new)

test_results/
├── elicitation_form_compliance_report.md (new)
└── elicitation_form_performance_report.md (new)
```

---

## Appendix B: Command Reference

### Development Commands

```bash
# Compile
TERM=dumb rebar3 compile

# Run all elicitation tests
rebar3 eunit --dir=apps/erlmcp_core/test --module=erlmcp_elicitation_*

# Run single module tests
rebar3 eunit --module=erlmcp_elicitation_form_tests

# Run Common Test suite
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_elicitation_integration_SUITE

# Generate coverage report
rebar3 cover --module=erlmcp_elicitation_form
rebar3 covertool generate

# Type checking
rebar3 dialyzer

# Cross-reference analysis
rebar3 xref

# Code formatting
rebar3 format --verify

# Full quality check
make check

# Performance benchmarks
make benchmark-quick
```

### Testing Workflow

```bash
# Chicago School TDD workflow (Red → Green → Refactor)

# 1. Write failing test
vim apps/erlmcp_core/test/erlmcp_elicitation_form_tests.erl

# 2. Run test (expect failure)
rebar3 eunit --module=erlmcp_elicitation_form_tests

# 3. Implement feature
vim apps/erlmcp_core/src/erlmcp_elicitation_form.erl

# 4. Run test (expect success)
rebar3 eunit --module=erlmcp_elicitation_form_tests

# 5. Refactor if needed
# 6. Verify all tests still pass
rebar3 eunit --dir=apps/erlmcp_core/test

# 7. Check coverage
rebar3 cover --module=erlmcp_elicitation_form
```

---

**Status:** Design Phase Complete
**Next Step:** Begin Phase 1 - Week 1 Implementation
**Owner:** erlang-otp-developer + erlang-test-engineer
**Reviewers:** erlang-architect, code-reviewer
