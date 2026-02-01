---
name: test-release-scout
description: Test suite for release-scout agent tool access and reporting
type: test
---

# Release Scout Agent - Test Suite

**Test File**: `.claude/agents/test_release_scout.md`
**Purpose**: Verify tool access constraints, WebSearch limits, and report format

## Test Cases

### TC-001: Tool Access - WebSearch Allowed

**Objective**: Verify WebSearch tool is available and functional

**Setup**:
- Agent: release-scout
- Task: Search for latest OTP release

**Test Steps**:
1. Request WebSearch on github.com (allowed domain)
2. Query: "erlang otp releases latest 2026"
3. Verify result returns release information

**Expected Result**: ✅ PASS
- WebSearch executes successfully
- Returns data from github.com
- Report generated in JSON format

**Actual Result**: _To be run during validation_

---

### TC-002: Tool Access - Bash Denied

**Objective**: Verify Bash execution is blocked

**Setup**:
- Agent: release-scout
- Task: Try to execute bash command

**Test Steps**:
1. Request Bash tool execution
2. Attempt: `git tag -l "OTP*"`
3. Verify denial

**Expected Result**: ✅ PASS
- Bash tool returns error
- Tool not in `toolAccess.allow` list
- Error message: "Tool 'Bash' not available for release-scout"

**Actual Result**: _To be run during validation_

---

### TC-003: Tool Access - Write Denied

**Objective**: Verify Write/Edit/Delete are blocked

**Setup**:
- Agent: release-scout
- Task: Try to create/modify file

**Test Steps**:
1. Request Write tool to create report file
2. Attempt: Create `/tmp/release-report.json`
3. Verify denial

**Expected Result**: ✅ PASS
- Write tool returns error
- Tool not in `toolAccess.allow` list
- Error message: "Tool 'Write' not available for release-scout"

**Actual Result**: _To be run during validation_

---

### TC-004: WebSearch Constraint - Allowed Domain (github.com)

**Objective**: Verify allowed domains work unrestricted

**Setup**:
- Agent: release-scout
- Domain: github.com (allowed)

**Test Steps**:
1. Execute WebSearch: site:github.com/erlang/otp "OTP-28.3"
2. Verify result

**Expected Result**: ✅ PASS
- Search executes without domain approval needed
- Returns release data from erlang/otp repository
- Source URL contains github.com

**Actual Result**: _To be run during validation_

---

### TC-005: WebSearch Constraint - Allowed Domain (hex.pm)

**Objective**: Verify hex.pm domain works for dependency discovery

**Setup**:
- Agent: release-scout
- Domain: hex.pm (allowed)

**Test Steps**:
1. Execute WebSearch: site:hex.pm "gun" latest version
2. Verify result

**Expected Result**: ✅ PASS
- Search executes on hex.pm
- Returns dependency version information
- No domain approval required

**Actual Result**: _To be run during validation_

---

### TC-006: WebSearch Constraint - Allowed Domain (erlang.org)

**Objective**: Verify erlang.org domain works for official information

**Setup**:
- Agent: release-scout
- Domain: erlang.org (allowed)

**Test Steps**:
1. Execute WebSearch: site:erlang.org "OTP 28.3" release
2. Verify result

**Expected Result**: ✅ PASS
- Search executes on erlang.org
- Returns official release information
- No domain approval required

**Actual Result**: _To be run during validation_

---

### TC-007: WebSearch Constraint - Blocked Domain (Generic)

**Objective**: Verify blocked domains require user approval

**Setup**:
- Agent: release-scout
- Domain: example.com (not in allowDomains)

**Test Steps**:
1. Request WebSearch on example.com
2. Verify askForOthers behavior

**Expected Result**: ✅ PASS
- Agent does NOT execute search automatically
- Prompts user: "Domain 'example.com' not in allowed list. Approve? (yes/no)"
- Returns pending user approval

**Actual Result**: _To be run during validation_

---

### TC-008: Report Format - Valid JSON (OTP Release)

**Objective**: Verify generated reports match JSON schema

**Setup**:
- Agent: release-scout
- Task: Search for OTP 28.3.x releases

**Test Steps**:
1. Execute search strategy: "Strategy 1: OTP Releases (Stable)"
2. Parse returned JSON report
3. Validate schema

**Expected Result**: ✅ PASS
- Report is valid JSON
- Contains required fields: type, name, current_version, new_version, release_date, changelog_url, status, recommendation, search_strategy, sources
- type = "otp"
- status = "stable" | "candidate" | "beta"
- recommendation = "upgrade" | "monitor" | "skip" | "urgent"
- All URLs are valid format

**Actual Result**: _To be run during validation_

---

### TC-009: Report Format - Valid JSON (Dependency Update)

**Objective**: Verify dependency reports match schema

**Setup**:
- Agent: release-scout
- Task: Search hex.pm for dependency updates

**Test Steps**:
1. Execute search strategy: "Strategy 2: Hex.pm Dependencies"
2. Parse returned JSON report
3. Validate schema

**Expected Result**: ✅ PASS
- Report is valid JSON
- type = "dependency"
- Contains package name and version info
- recommendation in [upgrade, monitor, skip]
- sources contain hex.pm URLs

**Actual Result**: _To be run during validation_

---

### TC-010: Report Format - Valid JSON (Release Candidate)

**Objective**: Verify RC/beta reports match schema

**Setup**:
- Agent: release-scout
- Task: Search for OTP release candidates

**Test Steps**:
1. Execute search strategy: "Strategy 3: Release Candidates"
2. Parse returned JSON report
3. Validate schema

**Expected Result**: ✅ PASS
- Report is valid JSON
- type = "candidate"
- status in [candidate, beta, alpha]
- breaking_changes field present
- recommendation typically "monitor"

**Actual Result**: _To be run during validation_

---

### TC-011: Report Format - Valid JSON (Security)

**Objective**: Verify security advisory reports match schema

**Setup**:
- Agent: release-scout
- Task: Search for security advisories

**Test Steps**:
1. Execute search strategy: "Strategy 4: Critical Security Updates"
2. Parse returned JSON report
3. Validate schema

**Expected Result**: ✅ PASS
- Report is valid JSON
- type = "security"
- status = "security-patch"
- recommendation = "urgent" | "high"
- CVE reference in notes or name

**Actual Result**: _To be run during validation_

---

### TC-012: Search Strategy - OTP Releases (Consistency)

**Objective**: Verify Strategy 1 consistently finds current releases

**Setup**:
- Agent: release-scout
- Strategy: "Strategy 1: OTP Releases (Stable)"

**Test Steps**:
1. Search: "OTP 28.3" on github.com/erlang/otp
2. Verify found version ≥ 28.3.1
3. Run search again
4. Compare results

**Expected Result**: ✅ PASS
- Both searches return same OTP version (no version regression)
- Release date reasonable (≤ today)
- Changelog URL accessible and valid

**Actual Result**: _To be run during validation_

---

### TC-013: Search Strategy - Hex.pm (Version Discovery)

**Objective**: Verify Strategy 2 finds latest hex packages

**Setup**:
- Agent: release-scout
- Strategy: "Strategy 2: Hex.pm Dependencies"
- Packages: jsx, gun, cowboy

**Test Steps**:
1. Search for jsx latest
2. Search for gun latest
3. Search for cowboy latest
4. Verify versions follow semver

**Expected Result**: ✅ PASS
- All packages found
- Versions in semver format (X.Y.Z)
- new_version ≥ current_version
- Sources contain hex.pm URLs

**Actual Result**: _To be run during validation_

---

### TC-014: Permission Mode - Read-Only Enforcement

**Objective**: Verify permissionMode prevents any modifications

**Setup**:
- Agent: release-scout
- permissionMode: read

**Test Steps**:
1. Attempt to write report to file
2. Attempt to modify .claude/agents/release-scout.md
3. Attempt to delete temporary files

**Expected Result**: ✅ PASS
- All write operations denied
- Error message references permissionMode: read
- No files modified during operation

**Actual Result**: _To be run during validation_

---

### TC-015: Model Configuration - Haiku Selection

**Objective**: Verify Haiku model appropriate for research task

**Setup**:
- Agent: release-scout
- Model: haiku

**Test Steps**:
1. Execute complex search query
2. Measure response time
3. Verify JSON parsing accuracy
4. Check output completeness

**Expected Result**: ✅ PASS
- Response time < 5 seconds per search
- Output complete and accurate
- JSON valid and well-formed
- Cost-effective for research workload

**Actual Result**: _To be run during validation_

---

### TC-016: YAML Frontmatter - Valid Structure

**Objective**: Verify agent definition is valid YAML

**Setup**:
- File: .claude/agents/release-scout.md

**Test Steps**:
1. Parse YAML frontmatter
2. Validate all required fields present
3. Check field types

**Expected Result**: ✅ PASS
- YAML parses without errors
- Required fields: name, description, model, toolAccess, webSearchConstraints
- toolAccess.allow contains Read, WebSearch
- toolAccess.deny contains Bash, Write, Edit, Delete
- webSearchConstraints.allowDomains includes github.com, hex.pm, erlang.org
- permissionMode = "read"

**Actual Result**: _To be run during validation_

---

### TC-017: Documentation - Search Strategies Clear

**Objective**: Verify documentation is complete and clear

**Setup**:
- File: .claude/agents/release-scout.md

**Test Steps**:
1. Read "Search Strategies" section
2. Verify each strategy has: Query Pattern, Search Focus, Data Points, Example
3. Verify 4 strategies documented

**Expected Result**: ✅ PASS
- 4 strategies documented (OTP, Hex.pm, RC, Security)
- Each has clear examples
- Query patterns use site: syntax
- Data points match report format

**Actual Result**: _To be run during validation_

---

### TC-018: Documentation - Report Format Complete

**Objective**: Verify report format documentation is comprehensive

**Setup**:
- File: .claude/agents/release-scout.md

**Test Steps**:
1. Read "Report Format" section
2. Verify Single Release Report JSON shown
3. Verify Batch Release Report JSON shown
4. Verify Field Definitions table
5. Verify Examples (4 scenarios)

**Expected Result**: ✅ PASS
- 4 examples provided (OTP, dependency, RC, security)
- Field definitions include type, name, version, recommendation
- JSON structure clear and parseable
- All examples valid JSON

**Actual Result**: _To be run during validation_

---

## Test Execution Summary

| Test ID | Test Name | Status | Notes |
|---------|-----------|--------|-------|
| TC-001 | WebSearch Allowed | ⏳ | Verify WebSearch works |
| TC-002 | Bash Denied | ⏳ | Verify tool blocking |
| TC-003 | Write Denied | ⏳ | Verify read-only |
| TC-004 | Domain: github.com | ⏳ | Allowed domain test |
| TC-005 | Domain: hex.pm | ⏳ | Allowed domain test |
| TC-006 | Domain: erlang.org | ⏳ | Allowed domain test |
| TC-007 | Domain: Blocked | ⏳ | Blocked domain test |
| TC-008 | Report: OTP JSON | ⏳ | Schema validation |
| TC-009 | Report: Dependency JSON | ⏳ | Schema validation |
| TC-010 | Report: Candidate JSON | ⏳ | Schema validation |
| TC-011 | Report: Security JSON | ⏳ | Schema validation |
| TC-012 | Strategy: OTP Consistency | ⏳ | Strategy test |
| TC-013 | Strategy: Hex.pm Discovery | ⏳ | Strategy test |
| TC-014 | Permission: Read-Only | ⏳ | Permission test |
| TC-015 | Model: Haiku Config | ⏳ | Model selection |
| TC-016 | YAML: Structure Valid | ⏳ | Format validation |
| TC-017 | Docs: Strategies Clear | ⏳ | Documentation |
| TC-018 | Docs: Report Format | ⏳ | Documentation |

**Total Tests**: 18
**Expected Passing**: 18/18
**Coverage Target**: ≥80%

---

## Notes

- Tests TC-001 through TC-015 require actual agent execution
- Tests TC-016 and TC-017 can be verified by inspection
- All WebSearch tests must use only allowed domains
- Report validation checks for valid JSON and field completeness
- Tool denial tests verify product-level enforcement

---

**Test Document Version**: 1.0
**Created**: 2026-02-01
**Status**: Ready for execution
