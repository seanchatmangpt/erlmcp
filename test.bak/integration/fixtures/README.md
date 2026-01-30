# TCPS Integration Test Fixtures

This directory contains test data fixtures for TCPS integration tests.

## Files

### sample_work_orders.json
Sample work orders covering all buckets (security, reliability, cost, compliance) with various priorities and metadata.

**Contents**:
- 3 security work orders (CVE patches, security enhancements)
- 2 reliability work orders (bug fixes, resilience improvements)
- 2 cost work orders (performance optimization, cost reduction)
- 2 compliance work orders (GDPR compliance, audit requirements)
- 2 feature work orders (GraphQL API, WebSocket support)

### sample_receipts.json
Sample receipts for all pipeline stages showing both success and failure scenarios.

**Contents**:
- SHACL validation receipts (valid and invalid)
- Compilation receipts (success and failure)
- Test receipts (passing with high coverage, passing with low coverage)
- Security scan receipts (clean and with vulnerabilities)
- Deterministic build receipts (reproducible and non-reproducible)
- Quality gate receipts (all gates passed)

### sample_andons.json
Sample Andon events covering different failure types and severities.

**Contents**:
- Test failure Andon (triggered)
- Compilation error Andon (triggered)
- Low coverage Andon (triggered)
- Security vulnerability Andon (resolved with 5 Whys)
- Non-deterministic build Andon (triggered)
- WIP limit exceeded Andon (resolved)
- SHACL violation Andon (triggered)

### sample_cve_advisories.json
Sample CVE advisories for security vulnerability testing.

**Contents**:
- 6 CVE advisories (2 critical, 3 high, 1 medium)
- SQL injection (CVE-2024-1234)
- OpenSSL vulnerability (CVE-2024-5678)
- HTTP request smuggling (CVE-2024-9999)
- XXE injection (CVE-2024-3333)
- JWT authentication bypass (CVE-2024-4444)
- Path traversal (CVE-2024-5555)

### sample_github_issues.json
Sample GitHub issues representing work order sources.

**Contents**:
- 5 GitHub issues (2 bugs, 3 feature requests)
- Memory leak bug (issue #4567)
- GraphQL API feature (issue #5432, 156 upvotes)
- Rate limiting security enhancement (issue #6789)
- Race condition bug (issue #7890)
- WebSocket feature (issue #8901, 89 upvotes)

## Usage

### Loading Fixtures in Tests

```erlang
%% Load all fixtures
Fixtures = load_all_fixtures().

%% Load specific fixture
WorkOrders = load_fixture("sample_work_orders.json").

%% Inject fixture data into mock services
lists:foreach(fun(WO) ->
    tcps_mock_services:inject_work_order(WO)
end, maps:get(security_work_orders, WorkOrders)).
```

### Creating Custom Fixtures

1. Copy an existing fixture file as a template
2. Modify the data to match your test scenario
3. Save with a descriptive name (e.g., `custom_security_scenario.json`)
4. Load in your test suite

### Fixture Data Standards

All fixture data follows these conventions:

- **IDs**: Use descriptive prefixes (e.g., `wo-sec-001`, `receipt-shacl-001`)
- **Timestamps**: Use Unix milliseconds (e.g., `1706297600000`)
- **Severity**: Use standard levels (`critical`, `high`, `medium`, `low`)
- **Status**: Use canonical states (`open`, `triggered`, `resolved`, `pass`, `fail`)

## Maintenance

When adding new fixtures:

1. Follow the existing JSON structure
2. Include comprehensive metadata
3. Cover both success and failure scenarios
4. Add documentation to this README
5. Validate JSON syntax before committing

## Integration with Tests

Fixtures are automatically loaded by `tcps_ct_hooks` during suite initialization. Each test suite can access fixture data through the Common Test config:

```erlang
init_per_suite(Config) ->
    %% Fixtures are loaded automatically by CT hooks
    {ok, Fixtures} = load_suite_fixtures(?MODULE),
    [{fixtures, Fixtures} | Config].
```
