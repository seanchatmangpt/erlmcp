# Security and Validation Features - erlmcp with Claude Code CLI

## Overview

This document documents how Claude Code CLI enhances the erlmcp project's security and validation capabilities, implementing comprehensive security controls throughout the MCP (Model Context Protocol) SDK. The system follows manufacturing-grade security standards with zero-tolerance policies for critical security violations.

## 1. Input Validation Patterns

### Multi-Layered Validation Architecture

#### Transport Layer Validation
```erlang
% HTTP Header Validation
-module(erlmcp_http_header_validator).
-export([validate_request_headers/2]).

validate_request_headers(Headers, Method) ->
    HeaderMap = maps:from_list([{string:lowercase(K), V} || {K, V} <- Headers]),
    validate_headers_by_method(HeaderMap, Method).

validate_headers_by_method(HeaderMap, post) ->
    Required = [{<<"content-type">>, fun validate_content_type/1}],
    validate_required_headers(HeaderMap, Required).

validate_content_type(<<"application/json">>) -> {ok, <<"application/json">>};
validate_content_type(<<"application/json; charset=utf-8">>) -> {ok, <<"application/json">>};
validate_content_type(_) -> {error, <<"Invalid Content-Type">>}.
```

#### Origin Validation for DNS Rebinding Protection
```erlang
-module(erlmcp_origin_validator).
-export([validate_origin/2]).

validate_origin(undefined, _Allowed) -> {ok, undefined};
validate_origin(Origin, Allowed) ->
    case match_wildcard_origin(Origin, Allowed) of
        true -> {ok, Origin};
        false -> {error, forbidden}
    end.

match_wildcard_origin(_Origin, <<"*">>) -> true;
match_wildcard_origin(Origin, <<"*.", Rest/binary>>) ->
    case binary:match(Origin, Rest) of
        {Pos, _} when Pos > 0 ->
            EndPart = binary:part(Origin, {byte_size(Origin) - byte_size(Rest), byte_size(Rest)}),
            EndPart =:= Rest;
        _ -> false
    end.
```

#### JSON Schema Validation
```erlang
-module(erlmcp_schema_validator).
-export([validate/3]).

validate(Worker, Schema, Data) ->
    gen_server:call(Worker, {validate, Schema, Data}, 5000).

do_validate(Schema, Data) ->
    case jesse:validate_with_schema(Schema, Data, [{allowed_errors, infinity}]) of
        {ok, _} -> ok;
        {error, Errors} -> {error, format_jesse_errors(Errors)}
    end.
```

### CLI Integration Example
```bash
# Validate MCP request schema
claude validate-mcp-request --input request.json --schema mcp_request_schema.json

# Validate origin headers
claude validate-origin --origin https://client.example.com --allowed-origins config/allowed_origins.txt

# Batch validation for multiple requests
claude batch-validate --input-dir requests/ --schema schemas/
```

## 2. Security Headers Integration

### Comprehensive HTTP Security Headers
```erlang
-module(erlmcp_security_headers).
-export([add_headers/1, add_headers/2, configure/1]).

add_headers(Headers) ->
    add_headers(Headers, #{}).

add_headers(Headers, Config) ->
    SecurityHeaders = build_security_headers(Config),
    merge_headers(Headers, SecurityHeaders).

build_security_headers(Config) ->
    [
        % Prevent MIME type sniffing
        {<<"x-content-type-options">>, <<"nosniff">>},

        % Prevent clickjacking
        {<<"x-frame-options">>, frame_options_value(Config)},

        % XSS protection
        {<<"x-xss-protection">>, <<"1; mode=block">>},

        % Content Security Policy
        {<<"content-security-policy">>, csp_value(Config)},

        % HSTS
        hsts_header(Config),

        % Referrer policy
        {<<"referrer-policy">>, referrer_policy_value(Config)},

        % Permissions policy
        {<<"permissions-policy">>, permissions_policy_value(Config)},

        % Certificate transparency
        {<<"expect-ct">>, <<"max-age=86400, enforce">>},

        % Cross-origin policies
        {<<"cross-origin-opener-policy">>, <<"same-origin">>},
        {<<"cross-origin-resource-policy">>, <<"same-origin">>},
        {<<"cross-origin-embedder-policy">>, <<"require-corp">>}
    ].
```

### CLI Security Header Management
```bash
# Configure security headers
claude security-headers --set csp "default-src 'self'; script-src 'self'"

# Generate security headers report
claude security-headers --report --output security-report.json

# Test security header effectiveness
claude security-headers --test --target https://localhost:8080
```

## 3. Secret Detection and Protection

### Multi-Backend Secrets Management
```erlang
-module(erlmcp_secrets).
-export([get_secret/1, set_secret/2, configure_vault/1, configure_aws/1]).

% Secret retrieval with caching
get_secret(Key) ->
    gen_server:call(?MODULE, {get_secret, Key}).

% Secret storage with encryption
set_secret(Key, Value) ->
    gen_server:call(?MODULE, {set_secret, Key, Value}).

% HashiCorp Vault integration
configure_vault(Config) ->
    gen_server:call(?MODULE, {configure_backend, vault, Config}).

% AWS Secrets Manager integration
configure_aws(Config) ->
    gen_server:call(?MODULE, {configure_backend, aws_secrets_manager, Config}).
```

### Secret Scanning and Detection
```bash
# Scan for hardcoded secrets
claude scan-secrets --path src/ --exclude test/

# Generate secret detection report
claude scan-secrets --output secret-report.json --severity critical

# Automatically remove detected secrets
claude scan-secrets --auto-remove --backup-dir backups/

# Interactive secret removal
claude scan-secrets --interactive --confirm-removal
```

## 4. Code Review Automation

### Automated Code Review with Security Focus
```bash
# Automated code review with security checks
claude review-code --target src/erlmcp_server.erl --focus security

# Generate security-focused review report
claude review-code --output security-review.json --checklist security-standards.yml

# Batch code review for multiple files
claude review-code --target-dir apps/ --report security-summary.md

# Real-time code review on save
claude watch --pattern "src/*.erl" --review-security
```

### Security Review Checklist Integration
```yaml
# security-standards.yml
security_checklist:
  - id: "SEC001"
    name: "Input Validation"
    check: "validate_all_inputs"
    severity: "critical"
    description: "All external inputs must be validated"

  - id: "SEC002"
    name: "Secret Management"
    check: "no_hardcoded_secrets"
    severity: "critical"
    description: "No hardcoded credentials or secrets"

  - id: "SEC003"
    name: "Error Handling"
    check: "secure_error_messages"
    severity: "high"
    description: "Error messages must not leak sensitive info"

  - id: "SEC004"
    name: "Authentication"
    check: "implement_auth"
    severity: "critical"
    description: "Mandatory authentication for all endpoints"
```

## 5. Compliance Checking Workflows

### Multi-Framework Compliance Validation
```erlang
% GDPR Compliance
log_user_data_consent(UserId, ConsentType) ->
    audit_log:log_consent(UserId, ConsentType),

% SOC2 Compliance
log_security_event(Event, UserId, Details) ->
    audit_log:log_security_event(Event, UserId, Details),

% HIPAA Compliance
log_patient_access(PatientId, UserId, AccessedRecords) ->
    audit_log:log_patient_access(PatientId, UserId, AccessedRecords).
```

### CLI Compliance Management
```bash
# Run compliance checks for GDPR
claude compliance check --standard gdpr --output gdpr-report.json

# Run SOC2 compliance
claude compliance check --standard soc2 --control A.12 --report soc2-a12.json

# Run HIPAA compliance
claude compliance check --standard hipaa --section 164.306 --output hipaa-report.json

# Generate compliance dashboard
claude compliance dashboard --standards gdpr,soc2,hipaa --output compliance-dashboard.html
```

## 6. Security Best Practices Implementation

### Authentication and Authorization
```erlang
-module(erlmcp_auth).
-export([authenticate/2, validate_jwt/1, check_permission/3]).

% Multiple authentication methods
authenticate(Token, Method) when Method =:= jwt ->
    validate_jwt(Token);
authenticate(Token, Method) when Method =:= api_key ->
    validate_api_key(Token);
authenticate(Token, Method) when Method =:= oauth2 ->
    validate_oauth2_token(Token);
authenticate(Token, Method) when Method =:= mtls ->
    validate_mtls(Token).

% JWT validation with jose library
validate_jwt(Token) ->
    case jose_jwt:verify_keys(PublicKeys, Token) of
        {true, Claims, _JWS} -> {ok, Claims};
        {false, _} -> {error, invalid_jwt}
    end.

% Role-based access control
check_permission(UserId, Resource, Action) ->
    case rbac:check_permission(UserId, Resource, Action) of
        true -> allow;
        false -> deny
    end.
```

### Secure Error Handling
```erlang
% Secure error message formatting
format_error(Reason) when is_binary(Reason) ->
    case is_sensitive_error(Reason) of
        true -> <<"Internal server error">>;
        false -> Reason
    end.

is_sensitive_error(Reason) ->
    lists:any(fun(Pattern) ->
        case binary:match(Reason, Pattern) of
            nomatch -> false;
            _ -> true
        end
    end, [<<"password">>, <<"secret">>, <<"key">>, <<"token">>]).

% Secure logging
log_error_with_context(Error, Context) ->
    SanitizedContext = sanitize_sensitive_data(Context),
    audit_log:log_error(Error, SanitizedContext).
```

## 7. Validation Gate Enforcement

### Quality Gate Security Integration
```bash
# Enhanced quality gate with security checks
./tools/quality-gate-enforcer.sh --include-security

# Security-specific quality gates
./tools/security-gate-enforcer.sh

# Continuous security monitoring
claude security-monitor --enable --alert-threshold high
```

### Multi-Layer Security Validation
```yaml
# security-gates.yml
security_gates:
  compilation:
    enabled: true
    blocking: true
    checks:
      - compile_without_errors
      - no_security_warnings

  dependencies:
    enabled: true
    blocking: true
    checks:
      - scan_vulnerabilities
      - update_patches

  tests:
    enabled: true
    blocking: true
    checks:
      - security_tests_pass
      - penetration_tests_pass

  code_quality:
    enabled: true
    blocking: true
    checks:
      - no_hardcoded_secrets
      - secure_error_handling
      - proper_authentication
```

## 8. Error Handling Security

### Secure Error Handling Patterns
```erlang
% Error handling with security considerations
handle_request(Request) ->
    try
        validate_request(Request),
        process_request(Request)
    catch
        throw:validation_error ->
            secure_error_response(400, "Invalid request");
        error:badarg ->
            secure_error_response(400, "Invalid arguments");
        error:timeout ->
            secure_error_response(408, "Request timeout");
        Error:Reason ->
            log_error(Error, Reason),
            secure_error_response(500, "Internal server error")
    end.

secure_error_response(Code, Message) ->
    % Remove any sensitive information from error response
    SanitizedMessage = sanitize_error_message(Message),
    {Code, #{error => SanitizedMessage}}.
```

## 9. Dependency Security Scanning

### Vulnerability Scanning Integration
```bash
# Scan dependencies for security vulnerabilities
claude scan-dependencies --output dependency-report.json

# Check for known CVEs
claude scan-cves --dependency-list deps/ --output cve-report.json

# Generate security bill of materials
claude sbom --output sbom.json --format cyclonedx

# Update vulnerable dependencies
claude update-vulnerable --auto-approve --backup
```

### Dependency Security Policy
```erlang
% Dependency version checking
check_dependency_security(Deps) ->
    Vulnerable = lists:filter(fun is_vulnerable/1, Deps),
    case Vulnerable of
        [] -> {ok, all_dependencies_secure};
        _ -> {error, vulnerable_found, Vulnerable}
    end.

is_vulnerable({Package, Version}) ->
    case vulnerability_db:check(Package, Version) of
        {ok, secure} -> false;
        {ok, vulnerable, CVEs} -> true
    end.
```

## 10. Audit Logging and Reporting

### Tamper-Proof Audit Logging
```erlang
-module(erlmcp_audit_log).
-export([log_auth_success/2, log_auth_failure/2, log_operation/4]).

% Immutable audit log with hash chain
log_auth_success(UserId, Metadata) ->
    gen_server:cast(?MODULE, {log, auth_success, #{
        user_id => UserId,
        result => success,
        metadata => Metadata
    }}).

% Log failed authentication attempts
log_auth_failure(undefined, Metadata) ->
    gen_server:cast(?MODULE, {log, auth_failure, #{
        user_id => undefined,
        result => failure,
        metadata => Metadata
    }}).

% Chain verification for tamper detection
verify_chain() ->
    verify_chain(current, verify).

verify_chain(Current, Previous) when is_binary(Current), is_binary(Previous) ->
    Current =:= crypto:hash(sha256, Previous).
```

### CLI Audit Management
```bash
# Generate audit report
claude audit-report --start-date 2024-01-01 --end-date 2024-12-31 --format pdf

# Export audit logs for compliance
claude audit-export --format json --output audit-logs.json

# Audit log integrity check
claude audit-verify --chain-hash-file hash-chain.json

# Real-time audit monitoring
claude audit-monitor --alert-on authentication --threshold 5
```

## Security Enhancement Workflow Example

### Complete Security Integration Example
```bash
# 1. Pre-commit security scan
claude security-precommit

# 2. Validate request schema
claude validate-mcp-request --input requests/test.json --schema schemas/mcp.json

# 3. Check for secrets
claude scan-secrets --path src/ --interactive

# 4. Run security tests
claude run-tests --suite security --coverage

# 5. Generate security report
claude security-report --output final-security-report.md

# 6. Compliance check
claude compliance check --standard gdpr --output compliance.json
```

## Security Metrics and Monitoring

### Security Metrics Collection
```erlang
% Security metrics collection
-export([collect_security_metrics/0]).

collect_security_metrics() ->
    #{
        auth_attempts => count_auth_attempts(),
        failed_logins => count_failed_logins(),
        brute_force_attempts => detect_brute_force(),
        vulnerable_requests => count_vulnerable_requests(),
        audit_log_size => get_audit_log_size(),
        security_events => count_security_events()
    }.
```

### CLI Security Dashboard
```bash
# Security metrics dashboard
claude security-dashboard --real-time --refresh 30s

# Alert configuration
claude security-alert --set auth_failures --threshold 5 --window 300s

# Historical security trends
claude security-trends --period 30d --output trends.csv
```

## Conclusion

The erlmcp project implements a comprehensive security framework enhanced by Claude Code CLI features. The multi-layered security approach ensures protection at all levels from transport to application logic, with automated enforcement of security best practices through quality gates and continuous monitoring.

Key features include:
- Multi-layer input validation
- Comprehensive security headers
- Secrets management with multiple backends
- Automated code review with security focus
- Compliance checking for multiple standards
- Secure error handling patterns
- Dependency vulnerability scanning
- Tamper-proof audit logging
- Real-time security monitoring

This implementation follows Zero Trust principles and defense-in-depth strategies to ensure the security and integrity of the MCP SDK.