%%%-----------------------------------------------------------------------------
%%% @doc MCP 2025-11-25 Security Analysis & Vulnerability Assessment
%%%
%%% This module provides formal analysis of MCP specification security
%%% properties and validates erlmcp implementation compliance.
%%%
%%% Generated from: docs/MCP_SPECIFICATION_COMPLETE.md
%%% Analysis Date: 2026-01-31
%%% Specification Version: 2025-11-25
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_mcp_security_analysis).

-export([
    analyze_implementation/1,
    validate_security_gates/1,
    assess_vulnerabilities/0,
    get_mitigation_requirements/0,
    validate_type_safety/1,
    validate_input_sanitization/1,
    validate_error_handling/1
]).

-export([
    security_checklist/0,
    type_safety_checklist/0,
    input_sanitization_checklist/0,
    error_handling_checklist/0,
    quality_gates/0
]).

-type vulnerability() :: #{
    category := binary(),
    severity := critical | high | medium | low,
    description := binary(),
    spec_reference := binary(),
    mitigation := binary()
}.

-type gate_result() :: #{
    gate := binary(),
    status := pass | fail | warning,
    details := [binary()],
    blockers := [binary()]
}.

%%%=============================================================================
%%% Security Vulnerabilities (Protocol Design)
%%%=============================================================================

-spec assess_vulnerabilities() -> [vulnerability()].
assess_vulnerabilities() ->
    [
        %% Authentication & Authorization
        #{
            category => <<"authentication">>,
            severity => critical,
            description => <<"No mandatory authentication requirement in spec">>,
            spec_reference => <<"Section 10.1: Authentication Methods">>,
            mitigation => <<"Require auth for all production deployments. "
                          "Default deny, explicit allow.">>
        },
        #{
            category => <<"authentication">>,
            severity => high,
            description => <<"JWT algorithm not restricted - allows 'none' attack">>,
            spec_reference => <<"Section 10.1: JWT mentioned without algorithm constraints">>,
            mitigation => <<"Restrict to RS256, ES256. Reject 'none' algorithm. "
                          "Validate signature before parsing claims.">>
        },
        #{
            category => <<"authorization">>,
            severity => high,
            description => <<"Authorization framework sparse - implementation dependent">>,
            spec_reference => <<"Section 10.2: Authorization Framework">>,
            mitigation => <<"Implement mandatory RBAC/ABAC. Document policy model. "
                          "Deny-by-default for all resources/tools.">>
        },

        %% Input Validation
        #{
            category => <<"input_validation">>,
            severity => high,
            description => <<"No max JSON depth - enables billion laughs attack">>,
            spec_reference => <<"Section 2.2: Message Requirements">>,
            mitigation => <<"Enforce max nesting depth (20 levels). "
                          "Reject deeply nested structures.">>
        },
        #{
            category => <<"input_validation">>,
            severity => high,
            description => <<"No string length limits - enables DoS">>,
            spec_reference => <<"Section 2.2: Message Requirements">>,
            mitigation => <<"Max string: 1MB. Max array: 10K elements. "
                          "Max object keys: 1K.">>
        },
        #{
            category => <<"input_validation">>,
            severity => high,
            description => <<"Numeric range not specified - overflow risk">>,
            spec_reference => <<"Line 126: IEEE 754 mentioned, no bounds">>,
            mitigation => <<"Validate safe integer range: -(2^53-1) to (2^53-1). "
                          "Reject Infinity, NaN.">>
        },
        #{
            category => <<"input_validation">>,
            severity => critical,
            description => <<"URI validation pattern not specified - path traversal risk">>,
            spec_reference => <<"Section 10.3: Input Validation">>,
            mitigation => <<"Canonicalize URIs. Reject '../', symlinks, "
                          "non-whitelisted schemes.">>
        },

        %% Transport Security
        #{
            category => <<"transport_security">>,
            severity => critical,
            description => <<"No TLS requirement for TCP transport">>,
            spec_reference => <<"Section 5.1: TCP transport">>,
            mitigation => <<"Mandatory TLS 1.3+ for production. "
                          "Certificate validation required.">>
        },
        #{
            category => <<"transport_security">>,
            severity => critical,
            description => <<"HTTP transport doesn't mandate HTTPS">>,
            spec_reference => <<"Section 5.1: HTTP + SSE transport">>,
            mitigation => <<"Require HTTPS for all HTTP transports. "
                          "HSTS headers mandatory.">>
        },
        #{
            category => <<"transport_security">>,
            severity => critical,
            description => <<"WebSocket doesn't mandate WSS">>,
            spec_reference => <<"Section 5.1: WebSocket transport">>,
            mitigation => <<"Require WSS (TLS). Reject WS in production.">>
        },
        #{
            category => <<"transport_security">>,
            severity => medium,
            description => <<"STDIO has no security model for multi-user systems">>,
            spec_reference => <<"Section 5.1: STDIO transport">>,
            mitigation => <<"Document: STDIO only for single-user, "
                          "local-trust environments.">>
        },

        %% Session Management
        #{
            category => <<"session_management">>,
            severity => high,
            description => <<"No session timeout specification">>,
            spec_reference => <<"Section 2.3: Connection States">>,
            mitigation => <<"Default 30min idle timeout. "
                          "Configurable 1min-24hr range.">>
        },
        #{
            category => <<"session_management">>,
            severity => high,
            description => <<"No session invalidation mechanism">>,
            spec_reference => <<"Section 2.3: Connection States">>,
            mitigation => <<"Add explicit logout/revoke method. "
                          "Clear all session state.">>
        },
        #{
            category => <<"session_management">>,
            severity => medium,
            description => <<"No session fixation prevention">>,
            spec_reference => <<"Section 3: Initialization">>,
            mitigation => <<"Regenerate session ID after auth. "
                          "Bind to client cert if mTLS.">>
        },

        %% Information Disclosure
        #{
            category => <<"information_disclosure">>,
            severity => medium,
            description => <<"Error 'data' field could leak internals">>,
            spec_reference => <<"Line 700-709: Error object structure">>,
            mitigation => <<"Sanitize error data. Never include stack traces, "
                          "file paths, env vars.">>
        },

        %% Resource Access
        #{
            category => <<"resource_access">>,
            severity => critical,
            description => <<"No URI canonicalization requirement">>,
            spec_reference => <<"Section 4.2: Resources capability">>,
            mitigation => <<"Canonicalize before access checks. "
                          "Resolve symlinks, normalize paths.">>
        },
        #{
            category => <<"resource_access">>,
            severity => high,
            description => <<"No whitelist for allowed URI schemes">>,
            spec_reference => <<"Section 4.2: Resources capability">>,
            mitigation => <<"Whitelist: file://, http://, https://. "
                          "Reject all others.">>
        },
        #{
            category => <<"resource_access">>,
            severity => high,
            description => <<"No symlink handling specification">>,
            spec_reference => <<"Section 4.2: Resources capability">>,
            mitigation => <<"Resolve symlinks before access checks. "
                          "Reject if outside allowed roots.">>
        },

        %% Denial of Service
        #{
            category => <<"denial_of_service">>,
            severity => high,
            description => <<"No connection limit specification">>,
            spec_reference => <<"Section 11.3: Scalability mentions 40-50K but not required">>,
            mitigation => <<"Enforce max connections per IP (100). "
                          "Global limit: 50K per node.">>
        },
        #{
            category => <<"denial_of_service">>,
            severity => high,
            description => <<"No request rate limiting requirement">>,
            spec_reference => <<"Refusal code 1056 mentioned but not required">>,
            mitigation => <<"Rate limit: 100 req/sec per session. "
                          "Burst: 200. Sliding window.">>
        },
        #{
            category => <<"denial_of_service">>,
            severity => high,
            description => <<"No concurrent request limit">>,
            spec_reference => <<"Section 2: Protocol Fundamentals">>,
            mitigation => <<"Max 10 concurrent requests per session. "
                          "Queue depth: 100.">>
        },
        #{
            category => <<"denial_of_service">>,
            severity => medium,
            description => <<"16 MB message size enables memory exhaustion">>,
            spec_reference => <<"Line 63: 16 MB default">>,
            mitigation => <<"Enforce per-connection memory limit (50 MB). "
                          "Kill connections exceeding budget.">>
        }
    ].

%%%=============================================================================
%%% Type Safety Requirements
%%%=============================================================================

-spec validate_type_safety(map()) -> gate_result().
validate_type_safety(Implementation) ->
    Checks = [
        {<<"UTF-8 validation">>,
         check_utf8_validation(Implementation)},
        {<<"IEEE 754 number handling">>,
         check_number_safety(Implementation)},
        {<<"JSON structure validation">>,
         check_json_structure(Implementation)},
        {<<"Type mismatch handling">>,
         check_type_mismatches(Implementation)},
        {<<"Null vs undefined">>,
         check_null_handling(Implementation)},
        {<<"Extra field handling">>,
         check_extra_fields(Implementation)},
        {<<"Base64 validation">>,
         check_base64_validation(Implementation)},
        {<<"MIME type validation">>,
         check_mime_validation(Implementation)},
        {<<"JSON Schema version">>,
         check_json_schema_version(Implementation)}
    ],

    Results = [{Check, Result} || {Check, Result} <- Checks],
    Failures = [Check || {Check, false} <- Results],

    #{
        gate => <<"Type Safety">>,
        status => if Failures =:= [] -> pass; true -> fail end,
        details => [Check || {Check, true} <- Results],
        blockers => Failures
    }.

%%%=============================================================================
%%% Input Sanitization Requirements
%%%=============================================================================

-spec validate_input_sanitization(map()) -> gate_result().
validate_input_sanitization(Implementation) ->
    Checks = [
        {<<"URI canonicalization">>,
         check_uri_canonicalization(Implementation)},
        {<<"URI scheme whitelist">>,
         check_uri_scheme_whitelist(Implementation)},
        {<<"Path traversal prevention">>,
         check_path_traversal(Implementation)},
        {<<"Symlink resolution">>,
         check_symlink_handling(Implementation)},
        {<<"JSON depth limit">>,
         check_json_depth(Implementation)},
        {<<"String length limits">>,
         check_string_limits(Implementation)},
        {<<"Array size limits">>,
         check_array_limits(Implementation)},
        {<<"Object key limits">>,
         check_object_limits(Implementation)},
        {<<"Numeric range validation">>,
         check_numeric_ranges(Implementation)},
        {<<"Method name validation">>,
         check_method_names(Implementation)},
        {<<"Control character filtering">>,
         check_control_chars(Implementation)},
        {<<"Unicode normalization">>,
         check_unicode_normalization(Implementation)},
        {<<"Message size enforcement">>,
         check_message_size(Implementation)}
    ],

    Results = [{Check, Result} || {Check, Result} <- Checks],
    Failures = [Check || {Check, false} <- Results],

    #{
        gate => <<"Input Sanitization">>,
        status => if Failures =:= [] -> pass; true -> fail end,
        details => [Check || {Check, true} <- Results],
        blockers => Failures
    }.

%%%=============================================================================
%%% Error Handling & Logging Requirements
%%%=============================================================================

-spec validate_error_handling(map()) -> gate_result().
validate_error_handling(Implementation) ->
    Checks = [
        {<<"All error codes implemented">>,
         check_error_codes(Implementation)},
        {<<"Refusal codes implemented">>,
         check_refusal_codes(Implementation)},
        {<<"Error data sanitization">>,
         check_error_sanitization(Implementation)},
        {<<"Consistent error messages">>,
         check_error_consistency(Implementation)},
        {<<"Exponential backoff support">>,
         check_backoff_support(Implementation)},
        {<<"Max retry limits">>,
         check_retry_limits(Implementation)},
        {<<"Timeout enforcement">>,
         check_timeouts(Implementation)},
        {<<"Progress tracking">>,
         check_progress_tracking(Implementation)},
        {<<"Logging capability">>,
         check_logging_capability(Implementation)},
        {<<"Sensitive data redaction">>,
         check_log_redaction(Implementation)},
        {<<"Structured logging">>,
         check_structured_logging(Implementation)}
    ],

    Results = [{Check, Result} || {Check, Result} <- Checks],
    Failures = [Check || {Check, false} <- Results],
    Warnings = [Check || {Check, undefined} <- Results],

    #{
        gate => <<"Error Handling & Logging">>,
        status => if
            Failures =:= [] andalso Warnings =:= [] -> pass;
            Failures =:= [] -> warning;
            true -> fail
        end,
        details => [Check || {Check, true} <- Results],
        blockers => Failures
    }.

%%%=============================================================================
%%% Quality Gates
%%%=============================================================================

-spec quality_gates() -> [#{atom() := term()}].
quality_gates() ->
    [
        #{
            name => <<"Security Gate">>,
            category => security,
            mandatory => true,
            criteria => [
                <<"Authentication enforced for production">>,
                <<"Authorization policy documented and enforced">>,
                <<"TLS 1.3+ required for network transports">>,
                <<"URI canonicalization implemented">>,
                <<"Path traversal prevention verified">>,
                <<"Rate limiting enforced">>,
                <<"Connection limits enforced">>,
                <<"Message size limits enforced">>,
                <<"Input validation comprehensive">>,
                <<"Error messages sanitized">>
            ],
            pass_threshold => 100
        },

        #{
            name => <<"Type Safety Gate">>,
            category => type_safety,
            mandatory => true,
            criteria => [
                <<"UTF-8 validation on all strings">>,
                <<"Safe integer range enforced">>,
                <<"JSON Schema validation for tool args">>,
                <<"Type mismatch errors returned">>,
                <<"Base64 validation for binary content">>,
                <<"MIME type validation">>,
                <<"Null handling consistent">>,
                <<"Extra field policy enforced">>
            ],
            pass_threshold => 100
        },

        #{
            name => <<"Input Sanitization Gate">>,
            category => sanitization,
            mandatory => true,
            criteria => [
                <<"JSON depth limited to 20 levels">>,
                <<"String length limited to 1 MB">>,
                <<"Array size limited to 10K elements">>,
                <<"Object keys limited to 1K">>,
                <<"URI scheme whitelist enforced">>,
                <<"Symlinks resolved and validated">>,
                <<"Control characters filtered">>,
                <<"Numeric ranges validated">>,
                <<"Method names validated">>
            ],
            pass_threshold => 100
        },

        #{
            name => <<"Error Handling Gate">>,
            category => error_handling,
            mandatory => true,
            criteria => [
                <<"All JSON-RPC 2.0 errors implemented">>,
                <<"All MCP error codes implemented">>,
                <<"All refusal codes implemented">>,
                <<"Error recovery patterns tested">>,
                <<"Timeouts enforced (5s request, 30s init)">>,
                <<"Progress tracking for long operations">>,
                <<"Exponential backoff documented">>,
                <<"Logging level control implemented">>,
                <<"Sensitive data redaction in logs">>
            ],
            pass_threshold => 100
        },

        #{
            name => <<"Protocol Compliance Gate">>,
            category => protocol,
            mandatory => true,
            criteria => [
                <<"Initialize method enforced first">>,
                <<"State machine validated">>,
                <<"Request ID uniqueness enforced">>,
                <<"Capability negotiation complete">>,
                <<"All declared capabilities implemented">>,
                <<"Notification delivery reliable">>,
                <<"Subscription cleanup on disconnect">>,
                <<"Session timeout enforced">>
            ],
            pass_threshold => 100
        },

        #{
            name => <<"Performance Gate">>,
            category => performance,
            mandatory => false,
            criteria => [
                <<"p99 latency < 1ms (in-memory)">>,
                <<"p99 latency < 100ms (network)">>,
                <<"Throughput > 40K msg/sec (TCP)">>,
                <<"Supports 40K concurrent connections">>,
                <<"Memory per connection < 5 MB">>,
                <<"No memory leaks over 24h">>,
                <<"CPU < 80% under sustained load">>
            ],
            pass_threshold => 80
        }
    ].

%%%=============================================================================
%%% Checklists
%%%=============================================================================

-spec security_checklist() -> [binary()].
security_checklist() ->
    [
        <<"✓ Authentication enforced (JWT RS256/ES256, mTLS, OAuth2)">>,
        <<"✓ Authorization RBAC/ABAC implemented with deny-by-default">>,
        <<"✓ TLS 1.3+ mandatory for TCP, HTTPS, WSS transports">>,
        <<"✓ Certificate validation enforced">>,
        <<"✓ Session timeout: 30min default (1min-24hr configurable)">>,
        <<"✓ Session invalidation/logout implemented">>,
        <<"✓ Session ID regeneration after auth">>,
        <<"✓ URI canonicalization before access checks">>,
        <<"✓ Path traversal prevention (reject ../, symlinks)">>,
        <<"✓ URI scheme whitelist (file://, http://, https://)">>,
        <<"✓ Symlink resolution and validation">>,
        <<"✓ Rate limiting: 100 req/sec per session, 200 burst">>,
        <<"✓ Connection limits: 100 per IP, 50K global">>,
        <<"✓ Concurrent request limit: 10 per session">>,
        <<"✓ Message size limit: 16 MB enforced">>,
        <<"✓ Per-connection memory limit: 50 MB">>,
        <<"✓ Error messages sanitized (no stack traces, paths)">>,
        <<"✓ Sensitive data redaction in logs">>,
        <<"✓ HSTS headers for HTTPS">>,
        <<"✓ CORS validation for HTTP transports">>
    ].

-spec type_safety_checklist() -> [binary()].
type_safety_checklist() ->
    [
        <<"✓ UTF-8 validation on all incoming strings">>,
        <<"✓ Invalid UTF-8 rejected with -32600">>,
        <<"✓ Safe integer range: -(2^53-1) to (2^53-1)">>,
        <<"✓ Infinity and NaN rejected">>,
        <<"✓ JSON Schema validation for tool arguments">>,
        <<"✓ JSON Schema version: Draft 2020-12">>,
        <<"✓ Type mismatches return -32602 Invalid params">>,
        <<"✓ Null vs undefined handling documented">>,
        <<"✓ Extra fields policy: ignored (liberal reading)">>,
        <<"✓ Base64 validation for image/audio content">>,
        <<"✓ MIME type validation against whitelist">>,
        <<"✓ Content type size limits enforced">>,
        <<"✓ IEEE 754 double precision for numbers">>,
        <<"✓ Field names as binaries (Erlang-specific)">>,
        <<"✓ Array homogeneity not required (JSON-RPC)">>
    ].

-spec input_sanitization_checklist() -> [binary()].
input_sanitization_checklist() ->
    [
        <<"✓ JSON nesting depth limited to 20 levels">>,
        <<"✓ String length limited to 1 MB">>,
        <<"✓ Array size limited to 10K elements">>,
        <<"✓ Object keys limited to 1K">>,
        <<"✓ URI canonicalization implemented">>,
        <<"✓ URI scheme whitelist enforced">>,
        <<"✓ Path traversal patterns rejected: ../, .\\, etc">>,
        <<"✓ Symlinks resolved and validated against roots">>,
        <<"✓ Method name pattern: ^[a-z][a-z0-9_/]*$ max 100 chars">>,
        <<"✓ Control characters filtered (0x00-0x1F except 0x09, 0x0A, 0x0D)">>,
        <<"✓ Unicode normalization: NFC form">>,
        <<"✓ Numeric range validation">>,
        <<"✓ Request ID type validation (number or string)">>,
        <<"✓ Request ID uniqueness per session">>,
        <<"✓ Cursor validation for pagination">>,
        <<"✓ Progress token validation">>,
        <<"✓ Task ID validation: ^task_[a-zA-Z0-9_-]+$">>,
        <<"✓ Protocol version format: YYYY-MM-DD">>
    ].

-spec error_handling_checklist() -> [binary()].
error_handling_checklist() ->
    [
        <<"✓ JSON-RPC 2.0 errors: -32700, -32600, -32601, -32602, -32603">>,
        <<"✓ MCP core errors: -32001 to -32010">>,
        <<"✓ Content errors: -32011 to -32020">>,
        <<"✓ Resource errors: -32021 to -32030">>,
        <<"✓ Tool errors: -32031 to -32040">>,
        <<"✓ Prompt errors: -32041 to -32050">>,
        <<"✓ Auth errors: -32051 to -32060">>,
        <<"✓ Protocol errors: -32061 to -32070">>,
        <<"✓ Pagination errors: -32071 to -32080">>,
        <<"✓ Task errors: -32081 to -32090">>,
        <<"✓ Progress errors: -32091 to -32100">>,
        <<"✓ Completion errors: -32110 to -32113">>,
        <<"✓ Refusal codes: 1001-1089 (bounded, documented)">>,
        <<"✓ Error data sanitization (no internals leaked)">>,
        <<"✓ Consistent error messages per code">>,
        <<"✓ Timeout enforcement: 5s requests, 30s init">>,
        <<"✓ Progress tracking for operations > 1s">>,
        <<"✓ Exponential backoff: 100ms, 200ms, 400ms, ...">>,
        <<"✓ Max retry limit: 5 attempts">>,
        <<"✓ Circuit breaker integration">>,
        <<"✓ Logging level control: debug, info, warn, error">>,
        <<"✓ Structured logging with correlation IDs">>,
        <<"✓ Sensitive data redaction: passwords, tokens, keys">>
    ].

%%%=============================================================================
%%% Stub Implementations (to be filled with actual checks)
%%%=============================================================================

%% Type safety checks
check_utf8_validation(_Impl) -> false.
check_number_safety(_Impl) -> false.
check_json_structure(_Impl) -> false.
check_type_mismatches(_Impl) -> false.
check_null_handling(_Impl) -> false.
check_extra_fields(_Impl) -> false.
check_base64_validation(_Impl) -> false.
check_mime_validation(_Impl) -> false.
check_json_schema_version(_Impl) -> false.

%% Input sanitization checks
check_uri_canonicalization(_Impl) -> false.
check_uri_scheme_whitelist(_Impl) -> false.
check_path_traversal(_Impl) -> false.
check_symlink_handling(_Impl) -> false.
check_json_depth(_Impl) -> false.
check_string_limits(_Impl) -> false.
check_array_limits(_Impl) -> false.
check_object_limits(_Impl) -> false.
check_numeric_ranges(_Impl) -> false.
check_method_names(_Impl) -> false.
check_control_chars(_Impl) -> false.
check_unicode_normalization(_Impl) -> false.
check_message_size(_Impl) -> false.

%% Error handling checks
check_error_codes(_Impl) -> false.
check_refusal_codes(_Impl) -> false.
check_error_sanitization(_Impl) -> false.
check_error_consistency(_Impl) -> false.
check_backoff_support(_Impl) -> false.
check_retry_limits(_Impl) -> false.
check_timeouts(_Impl) -> false.
check_progress_tracking(_Impl) -> false.
check_logging_capability(_Impl) -> false.
check_log_redaction(_Impl) -> false.
check_structured_logging(_Impl) -> false.

%% Main analysis functions
analyze_implementation(_Impl) ->
    #{
        vulnerabilities => assess_vulnerabilities(),
        mitigations => get_mitigation_requirements(),
        gates => quality_gates()
    }.

validate_security_gates(_Impl) ->
    #{status => not_implemented}.

get_mitigation_requirements() ->
    Vulns = assess_vulnerabilities(),
    [{maps:get(category, V), maps:get(mitigation, V)} || V <- Vulns].
