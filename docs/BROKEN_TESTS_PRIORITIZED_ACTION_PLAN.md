# Broken Tests Prioritized Action Plan

## Quick Reference Guide
This document provides a clear, actionable plan for fixing broken tests, prioritized by business value and compliance requirements.

## Priority Matrix

### 🔴 Critical (Must Fix - This Week)
**Risk if not fixed**: MCP non-compliance, security vulnerabilities, production failures

1. **Schema Validation Tests** (Score: 9/10)
   - **Files**: `src/erlmcp_schema_validator.erl.broken`, `test/erlmcp_schema_validator_tests.erl`
   - **Issue**: Core MCP 2025-11-25 compliance requirement
   - **Risk**: Tool definitions cannot be validated
   - **Action**: Restore schema validator using jesse library
   - **Deadline**: 3 days

2. **Prompt Argument Validator** (Score: 10/10)
   - **Files**: `src/erlmcp_prompt_argument_validator.erl.broken`
   - **Issue**: Gap #42 - Mandatory for MCP tool safety
   - **Risk**: Invalid arguments could cause tool failures
   - **Action**: Implement prompt argument validation
   - **Deadline**: 3 days

3. **URI Validator** (Score: 8/10)
   - **Files**: `src/erlmcp_uri_validator.erl.broken`
   - **Issue**: Security vulnerability - path traversal attacks
   - **Risk**: Resource registration security holes
   - **Action**: Restore URI validation with security checks
   - **Deadline**: 2 days

### 🟡 High Priority (Next Sprint)
**Risk if not fixed**: degraded performance, user experience issues

4. **TCP Transport Leaks** (Score: 7/10)
   - **Files**: `test/erlmcp_transport_tcp_leak_tests.erl.broken`
   - **Issue**: Memory leaks in primary transport
   - **Risk**: Production instability over time
   - **Action**: Fix leak tests or verify no leaks exist
   - **Deadline**: 1 week

5. **Connection Limiter Tests** (Score: 7/10)
   - **Files**: `test/erlmcp_connection_limiter_tests.erl.broken`
   - **Issue**: DoS protection gaps
   - **Risk**: Resource exhaustion attacks
   - **Action**: Enhance existing rate limiter coverage
   - **Deadline**: 1 week

6. **Rate Limiter v2 Integration** (Score: 6/10)
   - **Files**: `src/erlmcp_rate_limiter_v2.erl.broken`
   - **Issue**: Incomplete rate limiting features
   - **Risk**: Suboptimal rate limiting
   - **Action**: Merge v2 features into current rate limiter
   - **Deadline**: 1 week

### 🟢 Medium Priority (Post-MVP)
**Risk if not fixed**: Missing features, incomplete coverage

7. **Message Parser Coverage** (Score: 5/10)
   - **Files**: `test/erlmcp_message_parser_tests.erl.broken`
   - **Issue**: May be redundant
   - **Risk**: Duplication, maintenance burden
   - **Action**: Verify coverage elsewhere, remove if redundant
   - **Deadline**: 2 weeks

8. **Cancellation Tests** (Score: 6/10)
   - **Files**: `test/erlmcp_cancellation_tests.erl.broken`
   - **Issue**: MCP cancellation compliance
   - **Risk**: Poor user experience
   - **Action**: Verify coverage in integration tests
   - **Deadline**: 2 weeks

9. **Progress Tests** (Score: 5/10)
   - **Files**: `test/erlmcp_progress_tests.erl.broken`
   - **Issue**: User experience enhancement
   - **Risk**: Missing progress reporting
   - **Action**: Implement or verify coverage
   - **Deadline**: 2 weeks

### 🔵 Low Priority (Post-Launch)
**Risk if not fixed**: Technical debt, over-engineering

10. **Client Request ID Overflow** (Score: 4/10)
    - **Files**: `test/erlmcp_client_request_id_overflow_tests.erl.broken`
    - **Issue**: Edge case handling
    - **Risk**: Very rare failures
    - **Action**: Remove or merge with general tests
    - **Deadline**: Post-launch

11. **State Migration Tests** (Score: 3/10)
    - **Files**: Multiple migration test files
    - **Issue**: Advanced deployment features
    - **Risk**: Static deployments work fine
    - **Action**: Remove or defer
    - **Deadline**: Post-launch

12. **Code Reload Tests** (Score: 2/10)
    - **Files**: `test/erlmcp_code_reload_tests.erl.broken`
    - **Issue**: Development-time feature
    - **Risk**: None for production
    - **Action**: Remove
    - **Deadline**: Post-launch

## Implementation Strategy

### Phase 1: Critical Fixes (Week 1)
```bash
# Day 1-2: Security and Compliance
./scripts/fix_schema_validator.sh      # High impact, critical
./scripts/fix_uri_validator.sh        # Security vulnerability

# Day 2-3: Core Functionality
./scripts/fix_prompt_validator.sh     # MCP compliance requirement

# Day 3: Verification
rebar3 eunit --module=erlmcp_schema_validator_tests
rebar3 eunit --module=erlmcp_prompt_argument_validator_tests
rebar3 eunit --module=erlmcp_uri_validator_tests
```

### Phase 2: Production Hardening (Week 2)
```bash
# Day 4-5: Transport Stability
./scripts/fix_tcp_leak_tests.sh       # Prevent memory leaks
./scripts/enhance_rate_limiter.sh     # Better DoS protection

# Day 6-7: Security
./scripts/enhance_connection_limiter.sh # Protect against attacks
```

### Phase 3: Cleanup (Week 3)
```bash
# Remove low-priority broken tests
./scripts/cleanup_low_priority_tests.sh

# Verify no functionality lost
./scripts/run_compliance_checks.sh
```

## Success Metrics

### Critical Success Factors
- [ ] All schema validation tests pass (100%)
- [ ] No security vulnerabilities in URI validation
- [ ] Prompt argument validation working
- [ ] Memory leak tests passing

### Quality Gates
- [ ] 0 test failures in critical modules
- [ ] MCP 2025-11-25 compliance maintained
- [ ] No regression in existing functionality
- [ ] Code coverage maintained or improved

## Risk Mitigation

### High-Risk Items
1. **Schema Validation**: Has backup implementation plan using jesse
2. **URI Validator**: Has security audit checklist
3. **Prompt Validator**: Can use schema registry as foundation

### Dependencies
- jesse library dependency exists and working
- Current rate limiter can be extended
- Schema registry can be enhanced

## Decision Framework

### When to Stop
- Critical tests are fixed and passing
- Security vulnerabilities resolved
- MCP compliance maintained
- Production-ready codebase

### When to Continue
- If time allows, proceed to medium priority items
- If resources permit, address technical debt
- If new requirements emerge, reprioritize

## Communication Plan

### Stakeholder Updates
- Daily standups for critical fixes
- Weekly progress review
- Final signoff before deployment

### Documentation Updates
- Update test coverage reports
- Document new validation features
- Update security audit trail

This plan provides clear priorities and actionable steps to restore test coverage while focusing on business value and compliance requirements.