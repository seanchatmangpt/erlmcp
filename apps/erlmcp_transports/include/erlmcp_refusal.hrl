%%%-------------------------------------------------------------------
%% @doc Refusal Taxonomy - System-Wide Consistent Error Handling (v1.4.0)
%%
%% This header defines standardized refusal codes, HTTP status codes,
%% remediation hints, and severity levels for consistent error responses
%% across all erlmcp modules.
%%
%% Each refusal includes:
%% - error_code: Integer unique identifier
%% - http_status: HTTP status code (400, 401, 403, 404, 409, 413, 429, 503)
%% - remediation_hint: User-friendly actionable advice
%% - severity: warn | error | critical
%%
%% Usage in modules:
%%   -include("erlmcp_refusal.hrl").
%%   {error, Code, Status, Hint, Severity}
%%
%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_REFUSAL_HRL).
-define(ERLMCP_REFUSAL_HRL, 1).

%%====================================================================
%% Refusal Code Definitions (Unique Integers: 1001-1100)
%%====================================================================

%% Queue and Backpressure Refusals (1001-1010)
-define(REFUSAL_QUEUE_CAP_EXCEEDED, 1001).          % Message count exceeded
-define(REFUSAL_QUEUE_BYTE_CAP_EXCEEDED, 1002).     % Byte size limit exceeded
-define(REFUSAL_QUEUE_TENANT_CAP_EXCEEDED, 1003).   % Tenant aggregate limit exceeded
-define(REFUSAL_BUFFER_OVERFLOW, 1004).             % Internal buffer overflow
-define(REFUSAL_BACKPRESSURE_ACTIVE, 1005).         % Backpressure signal active

%% Authentication & Authorization Refusals (1011-1020)
-define(REFUSAL_AUTH_FAILED, 1011).                 % Authentication failed
-define(REFUSAL_AUTH_EXPIRED, 1012).                % Auth token expired
-define(REFUSAL_AUTH_INVALID_CREDENTIALS, 1013).    % Invalid credentials
-define(REFUSAL_AUTHZ_FORBIDDEN, 1014).             % Authorization denied
-define(REFUSAL_AUTH_MISSING, 1015).                % Missing authentication
-define(REFUSAL_SESSION_INVALID, 1016).             % Session ID invalid/missing

%% Parameter & Validation Refusals (1021-1035)
-define(REFUSAL_INVALID_PARAMS, 1021).              % Invalid request parameters
-define(REFUSAL_INVALID_JSON_SCHEMA, 1022).         % JSON schema validation failed
-define(REFUSAL_INVALID_URI, 1023).                 % URI format invalid
-define(REFUSAL_INVALID_CONTENT_TYPE, 1024).        % Content-Type not supported
-define(REFUSAL_INVALID_HEADER, 1025).              % Required header missing/invalid
-define(REFUSAL_INVALID_SESSION_ID, 1026).          % Session ID format invalid
-define(REFUSAL_INVALID_PROTOCOL_VERSION, 1027).    % Protocol version not supported
-define(REFUSAL_MISSING_REQUIRED_FIELD, 1028).      % Required field missing
-define(REFUSAL_FIELD_TYPE_MISMATCH, 1029).         % Field type doesn't match schema

%% Path & URI Security Refusals (1036-1045)
-define(REFUSAL_PATH_TRAVERSAL_DETECTED, 1036).     % Path traversal attack
-define(REFUSAL_PATH_INVALID, 1037).                % Path format invalid
-define(REFUSAL_SYMLINK_TRAVERSAL_DETECTED, 1038).  % Symlink traversal attack
-define(REFUSAL_URI_OUT_OF_BOUNDS, 1039).           % URI outside allowed roots
-define(REFUSAL_CANONICAL_PATH_VIOLATION, 1040).    % Canonical path validation failed

%% Resource & Entity Refusals (1046-1055)
-define(REFUSAL_RESOURCE_NOT_FOUND, 1046).          % Resource does not exist
-define(REFUSAL_RESOURCE_DUPLICATE, 1047).          % Resource already exists
-define(REFUSAL_TOOL_NOT_FOUND, 1048).              % Tool does not exist
-define(REFUSAL_TOOL_DUPLICATE, 1049).              % Tool already registered
-define(REFUSAL_PROMPT_NOT_FOUND, 1050).            % Prompt does not exist
-define(REFUSAL_PROMPT_DUPLICATE, 1051).            % Prompt already exists
-define(REFUSAL_ENTITY_DUPLICATE, 1052).            % Generic entity duplicate

%% Rate Limiting & Throttling Refusals (1056-1065)
-define(REFUSAL_RATE_LIMIT_EXCEEDED, 1056).         % Rate limit exceeded
-define(REFUSAL_RATE_LIMIT_PER_SECOND, 1057).       % Per-second rate limit hit
-define(REFUSAL_RATE_LIMIT_PER_MINUTE, 1058).       % Per-minute rate limit hit
-define(REFUSAL_QUOTA_EXCEEDED, 1059).              % Quota limit exceeded
-define(REFUSAL_CONCURRENT_LIMIT_EXCEEDED, 1060).   % Concurrent connection limit

%% Protocol & Transport Refusals (1066-1075)
-define(REFUSAL_PROTOCOL_ERROR, 1066).              % Protocol-level error
-define(REFUSAL_TRANSPORT_ERROR, 1067).             % Transport I/O error
-define(REFUSAL_MESSAGE_TOO_LARGE, 1068).           % Message size exceeds limit
-define(REFUSAL_TIMEOUT, 1069).                     % Operation timeout
-define(REFUSAL_UNSUPPORTED_ENCODING, 1070).        % Encoding not supported

%% Server State Refusals (1076-1085)
-define(REFUSAL_SERVER_UNINITIALIZED, 1076).        % Server not initialized
-define(REFUSAL_SERVER_SHUTTING_DOWN, 1077).        % Server shutting down
-define(REFUSAL_SERVICE_UNAVAILABLE, 1078).         % Service temporarily unavailable
-define(REFUSAL_INTERNAL_ERROR, 1079).              % Internal server error
-define(REFUSAL_DEPENDENCY_UNAVAILABLE, 1080).      % External dependency unavailable

%% Circuit Breaker & Health Refusals (1086-1095)
-define(REFUSAL_CIRCUIT_BREAKER_OPEN, 1086).        % Circuit breaker open
-define(REFUSAL_HEALTH_CHECK_FAILED, 1087).         % Health check failed
-define(REFUSAL_DEGRADED_SERVICE, 1088).            % Service degraded
-define(REFUSAL_RESOURCE_EXHAUSTED, 1089).          % Resource exhausted

%%====================================================================
%% HTTP Status Codes
%%====================================================================

-define(HTTP_200_OK, 200).
-define(HTTP_202_ACCEPTED, 202).
-define(HTTP_400_BAD_REQUEST, 400).
-define(HTTP_401_UNAUTHORIZED, 401).
-define(HTTP_403_FORBIDDEN, 403).
-define(HTTP_404_NOT_FOUND, 404).
-define(HTTP_409_CONFLICT, 409).
-define(HTTP_413_PAYLOAD_TOO_LARGE, 413).
-define(HTTP_415_UNSUPPORTED_MEDIA_TYPE, 415).
-define(HTTP_429_TOO_MANY_REQUESTS, 429).
-define(HTTP_503_SERVICE_UNAVAILABLE, 503).

%%====================================================================
%% Severity Levels
%%====================================================================

-define(SEVERITY_WARN, warn).
-define(SEVERITY_ERROR, error).
-define(SEVERITY_CRITICAL, critical).

%%====================================================================
%% Refusal Record Definition
%%====================================================================

-record(refusal, {
    code :: non_neg_integer(),              % Unique error code (1001-1100)
    http_status :: pos_integer(),           % HTTP status code
    message :: binary(),                    % Error message for user
    hint :: binary(),                       % Remediation hint/action
    severity :: warn | error | critical,    % Severity level
    details :: map() | undefined,           % Additional context (optional)
    timestamp :: integer()                  % Unix timestamp
}).

-type refusal() :: #refusal{}.
-type refusal_code() :: 1001..1095.

%%====================================================================
%% Convenience Macros for Common Refusal Creation
%%====================================================================

%% Queue Limit Refusals
-define(REFUSAL(Code), {error, Code}).
-define(REFUSAL_WITH_MSG(Code, Message), {error, Code, Message}).
-define(REFUSAL_WITH_DETAILS(Code, Details), {error, Code, Details}).

%%====================================================================
%% Refusal Lookup Table (Code -> Metadata)
%% Format: {Code, HTTPStatus, Message, Hint, Severity}
%%====================================================================

-define(REFUSAL_METADATA, [
    % Queue and Backpressure (1001-1005)
    {1001, ?HTTP_429_TOO_MANY_REQUESTS, <<"Queue capacity exceeded">>,
     <<"Reduce message rate or increase queue_limits.max_messages in config">>, error},
    {1002, ?HTTP_429_TOO_MANY_REQUESTS, <<"Byte capacity exceeded">>,
     <<"Message size too large or queue full; reduce message size or wait">>, error},
    {1003, ?HTTP_429_TOO_MANY_REQUESTS, <<"Tenant quota exceeded">>,
     <<"Tenant aggregate limit reached; contact administrator">>, critical},
    {1004, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Buffer overflow">>,
     <<"Internal buffer exhausted; reduce load or increase buffer_pool size">>, critical},
    {1005, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Backpressure active">>,
     <<"Server under load; retry after delay or reduce request rate">>, warn},

    % Authentication (1011-1016)
    {1011, ?HTTP_401_UNAUTHORIZED, <<"Authentication failed">>,
     <<"Provide valid credentials or check token format">>, error},
    {1012, ?HTTP_401_UNAUTHORIZED, <<"Authentication expired">>,
     <<"Refresh your authentication token">>, error},
    {1013, ?HTTP_401_UNAUTHORIZED, <<"Invalid credentials">>,
     <<"Check username, password, or API key">>, error},
    {1014, ?HTTP_403_FORBIDDEN, <<"Authorization denied">>,
     <<"You don't have permission for this resource; contact administrator">>, error},
    {1015, ?HTTP_401_UNAUTHORIZED, <<"Missing authentication">>,
     <<"Provide credentials via Authorization header or session ID">>, error},
    {1016, ?HTTP_401_UNAUTHORIZED, <<"Invalid session ID">>,
     <<"Provide valid MCP-Session-Id header (32+ hex chars)">>, error},

    % Parameters & Validation (1021-1029)
    {1021, ?HTTP_400_BAD_REQUEST, <<"Invalid parameters">>,
     <<"Check parameter names and types match API specification">>, error},
    {1022, ?HTTP_400_BAD_REQUEST, <<"JSON schema validation failed">>,
     <<"Review error details; structure must match schema">>, error},
    {1023, ?HTTP_400_BAD_REQUEST, <<"Invalid URI format">>,
     <<"Use valid absolute or relative URI (e.g., https://example.com/path)">>, error},
    {1024, ?HTTP_415_UNSUPPORTED_MEDIA_TYPE, <<"Invalid Content-Type">>,
     <<"Use application/json, text/plain, or application/octet-stream">>, error},
    {1025, ?HTTP_400_BAD_REQUEST, <<"Required header missing or invalid">>,
     <<"Include MCP-Protocol-Version and other required headers">>, error},
    {1026, ?HTTP_400_BAD_REQUEST, <<"Session ID invalid">>,
     <<"Session ID must be 32+ hexadecimal characters">>, error},
    {1027, ?HTTP_400_BAD_REQUEST, <<"Protocol version not supported">>,
     <<"Use protocol version 2025-11-25 or 2024-11-05">>, error},
    {1028, ?HTTP_400_BAD_REQUEST, <<"Required field missing">>,
     <<"Check request includes all mandatory fields per API spec">>, error},
    {1029, ?HTTP_400_BAD_REQUEST, <<"Field type mismatch">>,
     <<"Field type doesn't match schema; check API documentation">>, error},

    % Path & URI Security (1036-1040)
    {1036, ?HTTP_400_BAD_REQUEST, <<"Path traversal detected">>,
     <<"Remove .. and % sequences; use canonical paths">>, critical},
    {1037, ?HTTP_400_BAD_REQUEST, <<"Invalid path format">>,
     <<"Path must start with / and contain only valid characters">>, error},
    {1038, ?HTTP_400_BAD_REQUEST, <<"Symlink traversal detected">>,
     <<"Symlinks not allowed; use direct file paths">>, critical},
    {1039, ?HTTP_400_BAD_REQUEST, <<"URI out of bounds">>,
     <<"URI must resolve within configured roots">>, error},
    {1040, ?HTTP_400_BAD_REQUEST, <<"Canonical path violation">>,
     <<"Path must be canonical (no . or .. components)">>, error},

    % Resource & Entity (1046-1052)
    {1046, ?HTTP_404_NOT_FOUND, <<"Resource not found">>,
     <<"Check resource URI and ensure it exists">>, warn},
    {1047, ?HTTP_409_CONFLICT, <<"Resource already exists">>,
     <<"Use different URI or update existing resource">>, warn},
    {1048, ?HTTP_404_NOT_FOUND, <<"Tool not found">>,
     <<"Check tool name in tools/list or register new tool">>, warn},
    {1049, ?HTTP_409_CONFLICT, <<"Tool already registered">>,
     <<"Tool exists; use different name or unregister first">>, warn},
    {1050, ?HTTP_404_NOT_FOUND, <<"Prompt not found">>,
     <<"Check prompt name in prompts/list or create new prompt">>, warn},
    {1051, ?HTTP_409_CONFLICT, <<"Prompt already exists">>,
     <<"Prompt exists; use different name or update existing">>, warn},
    {1052, ?HTTP_409_CONFLICT, <<"Entity already exists">>,
     <<"Use different identifier or update via appropriate endpoint">>, warn},

    % Rate Limiting (1056-1060)
    {1056, ?HTTP_429_TOO_MANY_REQUESTS, <<"Rate limit exceeded">>,
     <<"Slow down request rate or implement exponential backoff">>, error},
    {1057, ?HTTP_429_TOO_MANY_REQUESTS, <<"Per-second rate limit exceeded">>,
     <<"Max 100 requests/sec; wait before retrying">>, error},
    {1058, ?HTTP_429_TOO_MANY_REQUESTS, <<"Per-minute rate limit exceeded">>,
     <<"Max 5000 requests/min; wait or increase plan">>, error},
    {1059, ?HTTP_429_TOO_MANY_REQUESTS, <<"Quota exceeded">>,
     <<"Monthly quota used; upgrade plan or wait for reset">>, error},
    {1060, ?HTTP_429_TOO_MANY_REQUESTS, <<"Concurrent connection limit exceeded">>,
     <<"Max 100 concurrent connections; close unused connections">>, error},

    % Protocol & Transport (1066-1070)
    {1066, ?HTTP_400_BAD_REQUEST, <<"Protocol error">>,
     <<"Review message format and protocol compliance">>, error},
    {1067, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Transport error">>,
     <<"Network error detected; check connection and retry">>, error},
    {1068, ?HTTP_413_PAYLOAD_TOO_LARGE, <<"Message too large">>,
     <<"Max 1MB per message; compress or split data">>, error},
    {1069, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Operation timeout">>,
     <<"Increase timeout or reduce server load">>, warn},
    {1070, ?HTTP_415_UNSUPPORTED_MEDIA_TYPE, <<"Encoding not supported">>,
     <<"Use UTF-8 or other supported encoding">>, error},

    % Server State (1076-1080)
    {1076, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Server not initialized">>,
     <<"Call initialize before other RPC methods">>, error},
    {1077, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Server shutting down">>,
     <<"Server is closing; reconnect after restart">>, warn},
    {1078, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Service unavailable">>,
     <<"Server temporarily unavailable; retry later">>, warn},
    {1079, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Internal error">>,
     <<"Unexpected error; check server logs and contact support">>, critical},
    {1080, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Dependency unavailable">>,
     <<"External service down; wait or contact support">>, warn},

    % Circuit Breaker & Health (1086-1089)
    {1086, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Circuit breaker open">>,
     <<"Too many failures; wait before retrying">>, warn},
    {1087, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Health check failed">>,
     <<"Server unhealthy; may be degraded or restarting">>, warn},
    {1088, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Service degraded">>,
     <<"Some features unavailable; functionality reduced">>, warn},
    {1089, ?HTTP_503_SERVICE_UNAVAILABLE, <<"Resource exhausted">>,
     <<"Server out of resources (memory, connections, etc.)">>, critical}
]).

-endif.
