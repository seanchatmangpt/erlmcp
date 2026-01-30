# Broken Tests Relevance Analysis

## Executive Summary
This document analyzes the relevance of broken tests in the erlmcp codebase. Tests are categorized by relevance score (0-10) and priority for remediation.

## Relevance Criteria
- **High (8-10)**: Tests verify critical, actively used functionality
- **Medium (5-7)**: Tests verify useful but non-critical functionality
- **Low (1-4)**: Tests verify deprecated, optional, or experimental features

## Analysis Methodology
1. Examine broken source files to understand functionality
2. Check current codebase for equivalent functionality
3. Assess criticality based on MCP protocol compliance
4. Consider test maintenance cost vs value

## Broken Tests Assessment

### 1. Schema Validation Tests
**Files**:
- `src/erlmcp_schema_validator.erl.broken`
- `test/erlmcp_schema_validator_tests.erl`

**Functionality**: JSON Schema validation using jesse library

**Current Status**:
- Schema registry (`erlmcp_schema_registry.erl`) exists but doesn't perform validation
- jesse library still referenced in dependencies
- Schema validation is a critical MCP compliance requirement

**Relevance Score**: 9/10 (HIGH)
- Critical for MCP 2025-11-25 compliance
- Schema validation is mandatory for tool/resource/prompt definitions
- jesse library dependency exists - validation should be working

**Recommendation**: **URGENT FIX REQUIRED** - Schema validation is core MCP functionality

### 2. Prompt Argument Validator Tests
**Files**:
- `src/erlmcp_prompt_argument_validator.erl.broken`
- Related tests in `test/`

**Functionality**: Validation of prompt arguments against declared schema

**Current Status**:
- No current equivalent functionality found
- Prompt argument validation is documented in MCP specification
- Gap #42 in documentation specifically mentions this

**Relevance Score**: 10/10 (HIGH)
- Direct requirement from MCP 2025-11-25 spec
- Essential for tool safety and correctness
- Documented implementation gap

**Recommendation**: **URGENT FIX REQUIRED** - Mandatory for MCP compliance

### 3. URI Validator Tests
**Files**:
- `src/erlmcp_uri_validator.erl.broken`
- Related tests in `test/`

**Functionality**: RFC 3986 URI validation with MCP-specific constraints

**Current Status**:
- URI validation still needed for resource registration
- No current equivalent implementation found
- Path traversal prevention is security-critical

**Relevance Score**: 8/10 (HIGH)
- Security-critical functionality
- Required for resource registration in MCP
- Path protection against attacks is essential

**Recommendation**: **HIGH PRIORITY FIX** - Security vulnerability without this

### 4. Rate Limiter v2 Tests
**Files**:
- `src/erlmcp_rate_limiter_v2.erl.broken`
- `test/erlmcp_rate_limiting_tests.erl` (may depend on v2 features)

**Functionality**: Enhanced rate limiting with versioned state migration

**Current Status**:
- Current `erlmcp_rate_limiter.erl` has version field (line 129)
- v2 features seem partially integrated
- Rate limiting is production-critical

**Relevance Score**: 6/10 (MEDIUM)
- Current implementation exists but may be incomplete
- v2 features appear to be migration-related
- Rate limiting itself is working (based on existing tests)

**Recommendation**: **MEDIUM PRIORITY** - Enhance existing rate limiter rather than restore v2

### 5. State Migration Tests
**Files**:
- Various `*_migration_tests.erl.broken`
- `test/erlmcp_state_migration_tests.erl`

**Functionality**: Hot code loading and state migration between versions

**Current Status**:
- State migration is advanced feature
- May not be needed in initial production deployment
- Most deployments use static deploys

**Relevance Score**: 3/10 (LOW)
- Advanced feature for hot deployments
- Not critical for initial production rollout
- Maintenance cost high, value moderate

**Recommendation**: **LOW PRIORITY** - Post-MVP enhancement

### 6. Message Parser Tests
**Files**:
- `test/erlmcp_message_parser_tests.erl.broken`

**Functionality**: JSON-RPC message parsing and validation

**Current Status**:
- JSON-RPC functionality likely implemented elsewhere
- Core communication protocol is working
- May be redundant with other test suites

**Relevance Score**: 5/10 (MEDIUM)
- JSON-RPC is fundamental to MCP
- May be covered by other test suites
- Parser errors would break core functionality

**Recommendation**: **MEDIUM PRIORITY** - Verify coverage elsewhere before removal

### 7. TCP Transport Leak Tests
**Files**:
- `test/erlmcp_transport_tcp_leak_tests.erl.broken`

**Functionality**: Memory leak detection in TCP transport

**Current Status**:
- TCP transport implemented and working
- Memory leaks could cause production issues
- May be covered by stress testing

**Relevance Score**: 7/10 (MEDIUM-HIGH)
- Memory leaks are critical for production stability
- TCP is primary transport for many deployments
- Stress tests may catch issues, but dedicated leak tests valuable

**Recommendation**: **HIGH PRIORITY** - Memory leaks are production blockers

### 8. Client Request ID Overflow Tests
**Files**:
- `test/erlmcp_client_request_id_overflow_tests.erl.broken`

**Functionality**: Test handling of request ID overflow scenarios

**Current Status**:
- Request correlation is working in current code
- Overflow handling is edge case but important
- May be covered by general request ID tests

**Relevance Score**: 4/10 (LOW-MEDIUM)
- Edge case that rarely occurs
- Current implementation likely handles it gracefully
- Maintenance cost for specialized test may not be worth it

**Recommendation**: **LOW PRIORITY** - Remove or merge with general request ID tests

### 9. Cancellation Tests
**Files**:
- `test/erlmcp_cancellation_tests.erl.broken`

**Functionality**: Test request cancellation functionality

**Current Status**:
- Cancellation is MCP standard feature
- May be implemented in current codebase
- Important for user experience

**Relevance Score**: 6/10 (MEDIUM)
- Important user experience feature
- MCP requires cancellation support
- May be covered by integration tests

**Recommendation**: **MEDIUM PRIORITY** - Verify coverage in integration tests

### 10. Progress Tests
**Files**:
- `test/erlmcp_progress_tests.erl.broken`

**Functionality**: Test progress reporting for long-running operations

**Current Status**:
- Progress reporting is MCP feature
- May be optional for initial deployment
- Good UX but not critical

**Relevance Score**: 5/10 (MEDIUM)
- Enhances user experience
- MCP specification includes progress reporting
- Can be implemented later

**Recommendation**: **MEDIUM PRIORITY** - Post-launch enhancement

### 11. Connection Limiter Tests
**Files**:
- `test/erlmcp_connection_limiter_tests.erl.broken`

**Functionality**: Test connection rate limiting

**Current Status**:
- Rate limiting is implemented in `erlmcp_rate_limiter.erl`
- Connection-specific limits may be included
- Protection against DoS attacks

**Relevance Score**: 7/10 (MEDIUM-HIGH)
- Security-critical functionality
- Current rate limiter may already cover this
- Protection against resource exhaustion

**Recommendation**: **HIGH PRIORITY** - Ensure DoS protection is complete

### 12. Code Reload Tests
**Files**:
- `test/erlmcp_code_reload_tests.erl.broken`

**Functionality**: Test hot code reloading capabilities

**Current Status**:
- Hot reloading is advanced feature
- Not needed for most production deployments
- Development-time feature

**Relevance Score**: 2/10 (LOW)
- Development-time feature
- Not critical for production
- High maintenance cost

**Recommendation**: **LOW PRIORITY** - Development-only feature

## Summary by Category

### High Priority (Fix Immediately)
- Schema Validation Tests (9/10) - Core MCP compliance
- Prompt Argument Validator (10/10) - Core MCP compliance
- URI Validator (8/10) - Security-critical
- TCP Transport Leak Tests (7/10) - Production stability
- Connection Limiter Tests (7/10) - Security/DoS protection

### Medium Priority (Fix in Next Sprint)
- Rate Limiter v2 (6/10) - Enhance existing
- Message Parser (5/10) - Verify coverage
- Cancellation Tests (6/10) - User experience
- Progress Tests (5/10) - User experience
- Client Request ID Overflow (4/10) - Edge case

### Low Priority (Post-MVP)
- State Migration Tests (3/10) - Advanced feature
- Code Reload Tests (2/10) - Development-only

## Recommendations

### Immediate Actions (This Week)
1. **Fix Schema Validation** - Mandatory for MCP compliance
2. **Fix Prompt Argument Validator** - Mandatory for MCP compliance
3. **Fix URI Validator** - Security vulnerability
4. **Fix TCP Transport Leaks** - Production stability
5. **Enhance Rate Limiter** - May replace v2 entirely

### Medium Term (Next Sprint)
1. Review cancellation and progress test coverage
2. Merge/eliminate redundant message parser tests
3. Enhance connection limiter coverage

### Long Term (Post-MVP)
1. Remove state migration tests (or implement later)
2. Remove code reload tests
3. Clean up low-priority broken tests

## Cost-Benefit Analysis

### High Cost, High Value
- Schema validation implementation: High cost, critical value
- URI validation: Medium cost, high security value

### Medium Cost, Medium Value
- Rate limiter enhancements: Medium cost, medium value
- Message parser cleanup: Low cost, medium value

### Low Cost, Low Value
- Test cleanup: Low cost, low value
- Edge case removal: Low cost, very low value

This analysis shows we should focus on the 5 high-priority items first, as they represent core MCP functionality and security requirements.