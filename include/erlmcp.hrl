-ifndef(ERLMCP_HRL).
-define(ERLMCP_HRL, 1).

%%% MCP Protocol Version
-define(APP_NAME, <<"erlmcp">>).
-define(MCP_VERSION, <<"2025-11-25">>).  %% Updated to latest MCP spec

%%% Standard JSON-RPC 2.0 Error Codes
-define(JSONRPC_PARSE_ERROR, -32700).
-define(JSONRPC_INVALID_REQUEST, -32600).
-define(JSONRPC_METHOD_NOT_FOUND, -32601).
-define(JSONRPC_INVALID_PARAMS, -32602).
-define(JSONRPC_INTERNAL_ERROR, -32603).

%%% JSON-RPC 2.0 Server Error Range (-32000 to -32099)
%%% These can be used for implementation-defined server errors
-define(JSONRPC_SERVER_ERROR_MIN, -32099).
-define(JSONRPC_SERVER_ERROR_MAX, -32000).

%%% JSON-RPC Protocol Fields
-define(JSONRPC_VERSION, <<"2.0">>).
-define(JSONRPC_FIELD_JSONRPC, <<"jsonrpc">>).
-define(JSONRPC_FIELD_ID, <<"id">>).
-define(JSONRPC_FIELD_METHOD, <<"method">>).
-define(JSONRPC_FIELD_PARAMS, <<"params">>).
-define(JSONRPC_FIELD_RESULT, <<"result">>).
-define(JSONRPC_FIELD_ERROR, <<"error">>).

%%% JSON-RPC Error Object Fields
-define(JSONRPC_ERROR_FIELD_CODE, <<"code">>).
-define(JSONRPC_ERROR_FIELD_MESSAGE, <<"message">>).
-define(JSONRPC_ERROR_FIELD_DATA, <<"data">>).

%%% MCP-Specific Error Codes (using the server error range -32000 to -32099)
%%% Per MCP 2025-11-25 specification and JSON-RPC 2.0

%% Core MCP errors (-32001 to -32010)
-define(MCP_ERROR_RESOURCE_NOT_FOUND, -32001).
-define(MCP_ERROR_TOOL_NOT_FOUND, -32002).
-define(MCP_ERROR_PROMPT_NOT_FOUND, -32003).
-define(MCP_ERROR_CAPABILITY_NOT_SUPPORTED, -32004).
-define(MCP_ERROR_NOT_INITIALIZED, -32005).
-define(MCP_ERROR_SUBSCRIPTION_FAILED, -32006).
-define(MCP_ERROR_VALIDATION_FAILED, -32007).
-define(MCP_ERROR_TRANSPORT_ERROR, -32008).
-define(MCP_ERROR_TIMEOUT, -32009).
-define(MCP_ERROR_RATE_LIMITED, -32010).

%% Content and message errors (-32011 to -32020)
-define(MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG, -32011).  %% Gap #40
-define(MCP_TOOL_DESCRIPTION_MAX_LENGTH_DEFAULT, 10000).  %% Default max tool description length
-define(MCP_ERROR_MESSAGE_TOO_LARGE, -32012).  %% Gap #45
-define(MCP_ERROR_INVALID_CONTENT_TYPE, -32013).
-define(MCP_ERROR_CONTENT_TOO_LARGE, -32014).
-define(MCP_ERROR_INVALID_ENCODING, -32015).
-define(MCP_ERROR_BINARY_DATA_TOO_LARGE, -32016).
-define(MCP_ERROR_TEXT_TOO_LONG, -32017).
-define(MCP_ERROR_INVALID_MIME_TYPE, -32018).
-define(MCP_ERROR_UNSUPPORTED_MEDIA_TYPE, -32019).
-define(MCP_ERROR_MEDIA_TYPE_NOT_ACCEPTABLE, -32020).

%% Resource and template errors (-32021 to -32030)
-define(MCP_ERROR_RESOURCE_TEMPLATE_NOT_FOUND, -32021).
-define(MCP_ERROR_INVALID_URI, -32022).
-define(MCP_ERROR_URI_SYNTAX_ERROR, -32023).
-define(MCP_ERROR_URI_TOO_LONG, -32024).
-define(MCP_ERROR_RESOURCE_ACCESS_DENIED, -32025).
-define(MCP_ERROR_RESOURCE_ALREADY_EXISTS, -32026).
-define(MCP_ERROR_RESOURCE_LOCKED, -32027).
-define(MCP_ERROR_RESOURCE_VERSION_MISMATCH, -32028).
-define(MCP_ERROR_TEMPLATE_RENDER_FAILED, -32029).
-define(MCP_ERROR_INVALID_URI_TEMPLATE, -32030).

%% Tool and execution errors (-32031 to -32040)
-define(MCP_ERROR_TOOL_EXECUTION_FAILED, -32031).
-define(MCP_ERROR_TOOL_TIMEOUT, -32032).
-define(MCP_ERROR_TOOL_CANCELLED, -32033).
-define(MCP_ERROR_INVALID_TOOL_ARGUMENTS, -32034).
-define(MCP_ERROR_TOOL_DISABLED, -32035).
-define(MCP_ERROR_TOOL_RESULT_TOO_LARGE, -32036).
-define(MCP_ERROR_TOOL_NOT_ALLOWED, -32037).
-define(MCP_ERROR_MAX_CONCURRENT_TOOLS, -32038).
-define(MCP_ERROR_TOOL_DEPENDENCY_FAILED, -32039).
-define(MCP_ERROR_TOOL_SCHEMA_INVALID, -32040).

%% Prompt and sampling errors (-32041 to -32050)
-define(MCP_ERROR_URL_ELICITATION_REQUIRED, -32042).  %% MCP 2025-11-25
-define(MCP_ERROR_PROMPT_ARGUMENT_MISSING, -32043).
-define(MCP_ERROR_PROMPT_RENDER_FAILED, -32044).
-define(MCP_ERROR_INVALID_PROMPT_ARGUMENTS, -32045).
-define(MCP_ERROR_SAMPLING_FAILED, -32046).
-define(MCP_ERROR_SAMPLING_TIMEOUT, -32047).
-define(MCP_ERROR_INVALID_MODEL_PREFERENCES, -32048).
-define(MCP_ERROR_MODEL_NOT_AVAILABLE, -32049).
-define(MCP_ERROR_SAMPLING_RATE_LIMITED, -32050).

%% Authentication and authorization errors (-32051 to -32060)
-define(MCP_ERROR_AUTHENTICATION_FAILED, -32051).
-define(MCP_ERROR_AUTHORIZATION_FAILED, -32052).
-define(MCP_ERROR_INVALID_CREDENTIALS, -32053).
-define(MCP_ERROR_TOKEN_EXPIRED, -32054).
-define(MCP_ERROR_INSUFFICIENT_PERMISSIONS, -32055).
-define(MCP_ERROR_ACCESS_DENIED, -32056).
-define(MCP_ERROR_SESSION_EXPIRED, -32057).
-define(MCP_ERROR_SESSION_NOT_FOUND, -32058).
-define(MCP_ERROR_INVALID_TOKEN, -32059).
-define(MCP_ERROR_UNAUTHORIZED_OPERATION, -32060).

%% Protocol and negotiation errors (-32061 to -32070)
-define(MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION, -32061).  %% Gap #30 (corrected)
-define(MCP_ERROR_PROTOCOL_VERSION_MISMATCH, -32062).
-define(MCP_ERROR_CAPABILITY_NEGOTIATION_FAILED, -32063).
-define(MCP_ERROR_INCOMPATIBLE_CAPABILITIES, -32064).
-define(MCP_ERROR_METHOD_NOT_SUPPORTED, -32065).
-define(MCP_ERROR_NOTIFICATION_NOT_SUPPORTED, -32066).
-define(MCP_ERROR_REQUEST_ID_INVALID, -32067).
-define(MCP_ERROR_REQUEST_ID_CONFLICT, -32068).
-define(MCP_ERROR_BATCH_REQUEST_TOO_LARGE, -32069).
-define(MCP_ERROR_BATCH_PARTIAL_FAILURE, -32070).

%% Pagination and cursor errors (-32071 to -32080)
-define(MCP_ERROR_INVALID_CURSOR, -32071).
-define(MCP_ERROR_CURSOR_EXPIRED, -32072).
-define(MCP_ERROR_PAGINATION_NOT_SUPPORTED, -32073).
-define(MCP_ERROR_PAGE_SIZE_TOO_LARGE, -32074).
-define(MCP_ERROR_PAGE_SIZE_INVALID, -32075).
-define(MCP_ERROR_INVALID_OFFSET, -32076).
-define(MCP_ERROR_CURSOR_REQUIRES_PARAMETER, -32077).
-define(MCP_ERROR_CURSOR_ENCODING_FAILED, -32078).
-define(MCP_ERROR_PAGINATION_LIMIT_EXCEEDED, -32079).
-define(MCP_ERROR_CURSOR_POSITION_INVALID, -32080).

%% Task and job errors (-32081 to -32090)
-define(MCP_ERROR_TASK_NOT_FOUND, -32081).
-define(MCP_ERROR_TASK_ALREADY_EXISTS, -32082).
-define(MCP_ERROR_TASK_FAILED, -32083).
-define(MCP_ERROR_TASK_CANCELLED, -32084).
-define(MCP_ERROR_TASK_TIMEOUT, -32085).
-define(MCP_ERROR_TASK_STATE_INVALID, -32086).
-define(MCP_ERROR_MAX_CONCURRENT_TASKS, -32087).
-define(MCP_ERROR_TASK_DEPENDENCY_FAILED, -32088).
-define(MCP_ERROR_TASK_RESULT_NOT_READY, -32089).
-define(MCP_ERROR_TASK_ALREADY_COMPLETED, -32090).

%% Progress and notification errors (-32091 to -32100)
-define(MCP_ERROR_INVALID_PROGRESS_TOKEN, -32091).
-define(MCP_ERROR_PROGRESS_TOKEN_EXPIRED, -32092).
-define(MCP_ERROR_PROGRESS_UPDATE_FAILED, -32093).
-define(MCP_ERROR_NOTIFICATION_FAILED, -32094).
-define(MCP_ERROR_NOTIFICATION_QUEUE_FULL, -32095).
-define(MCP_ERROR_INVALID_NOTIFICATION_TYPE, -32096).
-define(MCP_ERROR_NOTIFICATION_NOT_DELIVERED, -32097).
-define(MCP_ERROR_PROGRESS_VALUE_INVALID, -32098).
-define(MCP_ERROR_PROGRESS_TOTAL_INVALID, -32099).
-define(MCP_ERROR_CUSTOM_SERVER_ERROR, -32000).  %% Generic fallback

%%% Convenience list of all valid error codes for validation
%%% Includes JSON-RPC 2.0 standard codes and MCP-specific codes
-define(VALID_ERROR_CODES, [
    %% JSON-RPC 2.0 standard errors
    -32700,  % Parse error
    -32600,  % Invalid Request
    -32601,  % Method not found
    -32602,  % Invalid params
    -32603,  % Internal error
    %% Core MCP errors (-32001 to -32010)
    -32001,  % Resource not found
    -32002,  % Tool not found
    -32003,  % Prompt not found
    -32004,  % Capability not supported
    -32005,  % Not initialized
    -32006,  % Subscription failed
    -32007,  % Validation failed
    -32008,  % Transport error
    -32009,  % Timeout
    -32010,  % Rate limited
    %% Content and message errors (-32011 to -32020)
    -32011,  % Tool description too long
    -32012,  % Message too large
    -32013,  % Invalid content type
    -32014,  % Content too large
    -32015,  % Invalid encoding
    -32016,  % Binary data too large
    -32017,  % Text too long
    -32018,  % Invalid MIME type
    -32019,  % Unsupported media type
    -32020,  % Media type not acceptable
    %% Resource and template errors (-32021 to -32030)
    -32021,  % Resource template not found
    -32022,  % Invalid URI
    -32023,  % URI syntax error
    -32024,  % URI too long
    -32025,  % Resource access denied
    -32026,  % Resource already exists
    -32027,  % Resource locked
    -32028,  % Resource version mismatch
    -32029,  % Template render failed
    -32030,  % Invalid URI template
    %% Tool and execution errors (-32031 to -32040)
    -32031,  % Tool execution failed
    -32032,  % Tool timeout
    -32033,  % Tool cancelled
    -32034,  % Invalid tool arguments
    -32035,  % Tool disabled
    -32036,  % Tool result too large
    -32037,  % Tool not allowed
    -32038,  % Max concurrent tools
    -32039,  % Tool dependency failed
    -32040,  % Tool schema invalid
    %% Prompt and sampling errors (-32041 to -32050)
    -32042,  % URL elicitation required
    -32043,  % Prompt argument missing
    -32044,  % Prompt render failed
    -32045,  % Invalid prompt arguments
    -32046,  % Sampling failed
    -32047,  % Sampling timeout
    -32048,  % Invalid model preferences
    -32049,  % Model not available
    -32050,  % Sampling rate limited
    %% Authentication and authorization errors (-32051 to -32060)
    -32051,  % Authentication failed
    -32052,  % Authorization failed
    -32053,  % Invalid credentials
    -32054,  % Token expired
    -32055,  % Insufficient permissions
    -32056,  % Access denied
    -32057,  % Session expired
    -32058,  % Session not found
    -32059,  % Invalid token
    -32060,  % Unauthorized operation
    %% Protocol and negotiation errors (-32061 to -32070)
    -32061,  % Unsupported protocol version
    -32062,  % Protocol version mismatch
    -32063,  % Capability negotiation failed
    -32064,  % Incompatible capabilities
    -32065,  % Method not supported
    -32066,  % Notification not supported
    -32067,  % Request ID invalid
    -32068,  % Request ID conflict
    -32069,  % Batch request too large
    -32070,  % Batch partial failure
    %% Pagination and cursor errors (-32071 to -32080)
    -32071,  % Invalid cursor
    -32072,  % Cursor expired
    -32073,  % Pagination not supported
    -32074,  % Page size too large
    -32075,  % Page size invalid
    -32076,  % Invalid offset
    -32077,  % Cursor requires parameter
    -32078,  % Cursor encoding failed
    -32079,  % Pagination limit exceeded
    -32080,  % Cursor position invalid
    %% Task and job errors (-32081 to -32090)
    -32081,  % Task not found
    -32082,  % Task already exists
    -32083,  % Task failed
    -32084,  % Task cancelled
    -32085,  % Task timeout
    -32086,  % Task state invalid
    -32087,  % Max concurrent tasks
    -32088,  % Task dependency failed
    -32089,  % Task result not ready
    -32090,  % Task already completed
    %% Progress and notification errors (-32091 to -32100)
    -32091,  % Invalid progress token
    -32092,  % Progress token expired
    -32093,  % Progress update failed
    -32094,  % Notification failed
    -32095,  % Notification queue full
    -32096,  % Invalid notification type
    -32097,  % Notification not delivered
    -32098,  % Progress value invalid
    -32099,  % Progress total invalid
    -32000   % Custom server error (generic fallback)
]).

%%% Error Messages (for consistency)
%%% JSON-RPC 2.0 Standard Error Messages
-define(JSONRPC_MSG_PARSE_ERROR, <<"Parse error">>).
-define(JSONRPC_MSG_INVALID_REQUEST, <<"Invalid Request">>).
-define(JSONRPC_MSG_METHOD_NOT_FOUND, <<"Method not found">>).
-define(JSONRPC_MSG_INVALID_PARAMS, <<"Invalid params">>).
-define(JSONRPC_MSG_INTERNAL_ERROR, <<"Internal error">>).

%%% Core MCP Error Messages (-32001 to -32010)
-define(MCP_MSG_RESOURCE_NOT_FOUND, <<"Resource not found">>).
-define(MCP_MSG_TOOL_NOT_FOUND, <<"Tool not found">>).
-define(MCP_MSG_PROMPT_NOT_FOUND, <<"Prompt not found">>).
-define(MCP_MSG_CAPABILITY_NOT_SUPPORTED, <<"Capability not supported">>).
-define(MCP_MSG_NOT_INITIALIZED, <<"Server not initialized">>).
-define(MCP_MSG_SUBSCRIPTION_FAILED, <<"Subscription failed">>).
-define(MCP_MSG_VALIDATION_FAILED, <<"Validation failed">>).
-define(MCP_MSG_TRANSPORT_ERROR, <<"Transport error">>).
-define(MCP_MSG_TIMEOUT, <<"Request timeout">>).
-define(MCP_MSG_RATE_LIMITED, <<"Rate limit exceeded">>).

%%% Content and Message Error Messages (-32011 to -32020)
-define(MCP_MSG_TOOL_DESCRIPTION_TOO_LONG, <<"Tool description exceeds maximum length">>).
-define(MCP_MSG_MESSAGE_TOO_LARGE, <<"Message size exceeds maximum allowed">>).
-define(MCP_MSG_INVALID_CONTENT_TYPE, <<"Invalid content type">>).
-define(MCP_MSG_CONTENT_TOO_LARGE, <<"Content too large">>).
-define(MCP_MSG_INVALID_ENCODING, <<"Invalid encoding">>).
-define(MCP_MSG_BINARY_DATA_TOO_LARGE, <<"Binary data too large">>).
-define(MCP_MSG_TEXT_TOO_LONG, <<"Text too long">>).
-define(MCP_MSG_INVALID_MIME_TYPE, <<"Invalid MIME type">>).
-define(MCP_MSG_UNSUPPORTED_MEDIA_TYPE, <<"Unsupported media type">>).
-define(MCP_MSG_MEDIA_TYPE_NOT_ACCEPTABLE, <<"Media type not acceptable">>).

%%% Resource and Template Error Messages (-32021 to -32030)
-define(MCP_MSG_RESOURCE_TEMPLATE_NOT_FOUND, <<"Resource template not found">>).
-define(MCP_MSG_INVALID_URI, <<"Invalid URI">>).
-define(MCP_MSG_URI_SYNTAX_ERROR, <<"URI syntax error">>).
-define(MCP_MSG_URI_TOO_LONG, <<"URI too long">>).
-define(MCP_MSG_RESOURCE_ACCESS_DENIED, <<"Resource access denied">>).
-define(MCP_MSG_RESOURCE_ALREADY_EXISTS, <<"Resource already exists">>).
-define(MCP_MSG_RESOURCE_LOCKED, <<"Resource locked">>).
-define(MCP_MSG_RESOURCE_VERSION_MISMATCH, <<"Resource version mismatch">>).
-define(MCP_MSG_TEMPLATE_RENDER_FAILED, <<"Template render failed">>).
-define(MCP_MSG_INVALID_URI_TEMPLATE, <<"Invalid URI template">>).

%%% Tool and Execution Error Messages (-32031 to -32040)
-define(MCP_MSG_TOOL_EXECUTION_FAILED, <<"Tool execution failed">>).
-define(MCP_MSG_TOOL_TIMEOUT, <<"Tool execution timeout">>).
-define(MCP_MSG_TOOL_CANCELLED, <<"Tool execution cancelled">>).
-define(MCP_MSG_INVALID_TOOL_ARGUMENTS, <<"Invalid tool arguments">>).
-define(MCP_MSG_TOOL_DISABLED, <<"Tool is disabled">>).
-define(MCP_MSG_TOOL_RESULT_TOO_LARGE, <<"Tool result too large">>).
-define(MCP_MSG_TOOL_NOT_ALLOWED, <<"Tool not allowed">>).
-define(MCP_MSG_MAX_CONCURRENT_TOOLS, <<"Maximum concurrent tools exceeded">>).
-define(MCP_MSG_TOOL_DEPENDENCY_FAILED, <<"Tool dependency failed">>).
-define(MCP_MSG_TOOL_SCHEMA_INVALID, <<"Tool schema invalid">>).

%%% Prompt and Sampling Error Messages (-32041 to -32050)
-define(MCP_MSG_URL_ELICITATION_REQUIRED, <<"URL elicitation required">>).
-define(MCP_MSG_PROMPT_ARGUMENT_MISSING, <<"Prompt argument missing">>).
-define(MCP_MSG_PROMPT_RENDER_FAILED, <<"Prompt render failed">>).
-define(MCP_MSG_INVALID_PROMPT_ARGUMENTS, <<"Invalid prompt arguments">>).
-define(MCP_MSG_SAMPLING_FAILED, <<"Sampling failed">>).
-define(MCP_MSG_SAMPLING_TIMEOUT, <<"Sampling timeout">>).
-define(MCP_MSG_INVALID_MODEL_PREFERENCES, <<"Invalid model preferences">>).
-define(MCP_MSG_MODEL_NOT_AVAILABLE, <<"Model not available">>).
-define(MCP_MSG_SAMPLING_RATE_LIMITED, <<"Sampling rate limited">>).

%%% Authentication and Authorization Error Messages (-32051 to -32060)
-define(MCP_MSG_AUTHENTICATION_FAILED, <<"Authentication failed">>).
-define(MCP_MSG_AUTHORIZATION_FAILED, <<"Authorization failed">>).
-define(MCP_MSG_INVALID_CREDENTIALS, <<"Invalid credentials">>).
-define(MCP_MSG_TOKEN_EXPIRED, <<"Token expired">>).
-define(MCP_MSG_INSUFFICIENT_PERMISSIONS, <<"Insufficient permissions">>).
-define(MCP_MSG_ACCESS_DENIED, <<"Access denied">>).
-define(MCP_MSG_SESSION_EXPIRED, <<"Session expired">>).
-define(MCP_MSG_SESSION_NOT_FOUND, <<"Session not found">>).
-define(MCP_MSG_INVALID_TOKEN, <<"Invalid token">>).
-define(MCP_MSG_UNAUTHORIZED_OPERATION, <<"Unauthorized operation">>).

%%% Protocol and Negotiation Error Messages (-32061 to -32070)
-define(MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION, <<"Unsupported protocol version">>).
-define(MCP_MSG_PROTOCOL_VERSION_MISMATCH, <<"Protocol version mismatch">>).
-define(MCP_MSG_CAPABILITY_NEGOTIATION_FAILED, <<"Capability negotiation failed">>).
-define(MCP_MSG_INCOMPATIBLE_CAPABILITIES, <<"Incompatible capabilities">>).
-define(MCP_MSG_METHOD_NOT_SUPPORTED, <<"Method not supported">>).
-define(MCP_MSG_NOTIFICATION_NOT_SUPPORTED, <<"Notification not supported">>).
-define(MCP_MSG_REQUEST_ID_INVALID, <<"Invalid request ID">>).
-define(MCP_MSG_REQUEST_ID_CONFLICT, <<"Request ID conflict">>).
-define(MCP_MSG_BATCH_REQUEST_TOO_LARGE, <<"Batch request too large">>).
-define(MCP_MSG_BATCH_PARTIAL_FAILURE, <<"Batch partial failure">>).

%%% Pagination and Cursor Error Messages (-32071 to -32080)
-define(MCP_MSG_INVALID_CURSOR, <<"Invalid cursor">>).
-define(MCP_MSG_CURSOR_EXPIRED, <<"Cursor expired">>).
-define(MCP_MSG_PAGINATION_NOT_SUPPORTED, <<"Pagination not supported">>).
-define(MCP_MSG_PAGE_SIZE_TOO_LARGE, <<"Page size too large">>).
-define(MCP_MSG_PAGE_SIZE_INVALID, <<"Invalid page size">>).
-define(MCP_MSG_INVALID_OFFSET, <<"Invalid offset">>).
-define(MCP_MSG_CURSOR_REQUIRES_PARAMETER, <<"Cursor requires parameter">>).
-define(MCP_MSG_CURSOR_ENCODING_FAILED, <<"Cursor encoding failed">>).
-define(MCP_MSG_PAGINATION_LIMIT_EXCEEDED, <<"Pagination limit exceeded">>).
-define(MCP_MSG_CURSOR_POSITION_INVALID, <<"Cursor position invalid">>).

%%% Task and Job Error Messages (-32081 to -32090)
-define(MCP_MSG_TASK_NOT_FOUND, <<"Task not found">>).
-define(MCP_MSG_TASK_ALREADY_EXISTS, <<"Task already exists">>).
-define(MCP_MSG_TASK_FAILED, <<"Task failed">>).
-define(MCP_MSG_TASK_CANCELLED, <<"Task cancelled">>).
-define(MCP_MSG_TASK_TIMEOUT, <<"Task timeout">>).
-define(MCP_MSG_TASK_STATE_INVALID, <<"Task state invalid">>).
-define(MCP_MSG_MAX_CONCURRENT_TASKS, <<"Maximum concurrent tasks exceeded">>).
-define(MCP_MSG_TASK_DEPENDENCY_FAILED, <<"Task dependency failed">>).
-define(MCP_MSG_TASK_RESULT_NOT_READY, <<"Task result not ready">>).
-define(MCP_MSG_TASK_ALREADY_COMPLETED, <<"Task already completed">>).

%%% Progress and Notification Error Messages (-32091 to -32100)
-define(MCP_MSG_INVALID_PROGRESS_TOKEN, <<"Invalid progress token">>).
-define(MCP_MSG_PROGRESS_TOKEN_EXPIRED, <<"Progress token expired">>).
-define(MCP_MSG_PROGRESS_UPDATE_FAILED, <<"Progress update failed">>).
-define(MCP_MSG_NOTIFICATION_FAILED, <<"Notification failed">>).
-define(MCP_MSG_NOTIFICATION_QUEUE_FULL, <<"Notification queue full">>).
-define(MCP_MSG_INVALID_NOTIFICATION_TYPE, <<"Invalid notification type">>).
-define(MCP_MSG_NOTIFICATION_NOT_DELIVERED, <<"Notification not delivered">>).
-define(MCP_MSG_PROGRESS_VALUE_INVALID, <<"Progress value invalid">>).
-define(MCP_MSG_PROGRESS_TOTAL_INVALID, <<"Progress total invalid">>).
-define(MCP_MSG_CUSTOM_SERVER_ERROR, <<"Custom server error">>).

%%% Legacy Parameter Error Messages (kept for backward compatibility)
-define(MCP_MSG_MISSING_URI_PARAMETER, <<"Missing uri parameter">>).
-define(MCP_MSG_MISSING_TOOL_NAME, <<"Missing tool name">>).
-define(MCP_MSG_MISSING_PROMPT_NAME, <<"Missing prompt name">>).

%%% Model Preferences Validation Messages
-define(MCP_MSG_INVALID_TEMPERATURE, <<"Invalid temperature">>).
-define(MCP_MSG_INVALID_MAX_TOKENS, <<"Invalid maxTokens">>).
-define(MCP_MSG_INVALID_STOP_SEQUENCES, <<"Invalid stopSequences">>).

%%% Model Preferences Parameter Names
-define(MCP_PARAM_MODEL_PREFERENCES, <<"modelPreferences">>).
-define(MCP_PARAM_COST_PRIORITY, <<"costPriority">>).
-define(MCP_PARAM_SPEED_PRIORITY, <<"speedPriority">>).
-define(MCP_PARAM_INTELLIGENCE_PRIORITY, <<"intelligencePriority">>).
-define(MCP_PARAM_TEMPERATURE, <<"temperature">>).
-define(MCP_PARAM_MAX_TOKENS, <<"maxTokens">>).
-define(MCP_PARAM_STOP_SEQUENCES, <<"stopSequences">>).

%%% Legacy Parameter Error Messages (kept for backward compatibility)

%%% Convenience macros for common error responses
-define(ERROR_PARSE, {?JSONRPC_PARSE_ERROR, ?JSONRPC_MSG_PARSE_ERROR}).
-define(ERROR_INVALID_REQUEST, {?JSONRPC_INVALID_REQUEST, ?JSONRPC_MSG_INVALID_REQUEST}).
-define(ERROR_METHOD_NOT_FOUND, {?JSONRPC_METHOD_NOT_FOUND, ?JSONRPC_MSG_METHOD_NOT_FOUND}).
-define(ERROR_INVALID_PARAMS, {?JSONRPC_INVALID_PARAMS, ?JSONRPC_MSG_INVALID_PARAMS}).
-define(ERROR_INTERNAL, {?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR}).

-define(ERROR_RESOURCE_NOT_FOUND, {?MCP_ERROR_RESOURCE_NOT_FOUND, ?MCP_MSG_RESOURCE_NOT_FOUND}).
-define(ERROR_TOOL_NOT_FOUND, {?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND}).
-define(ERROR_PROMPT_NOT_FOUND, {?MCP_ERROR_PROMPT_NOT_FOUND, ?MCP_MSG_PROMPT_NOT_FOUND}).

%%% Phase State Machine Constants (Gap #4: Initialization Phase Machine)
-define(MCP_DEFAULT_INIT_TIMEOUT_MS, 30000).  % 30 seconds default
-define(MCP_PHASE_INITIALIZATION, initialization).
-define(MCP_PHASE_INITIALIZED, initialized).
-define(MCP_PHASE_DISCONNECTED, disconnected).
-define(MCP_PHASE_CLOSED, closed).

%%% Phase Violation Error Messages
-define(MCP_MSG_PHASE_VIOLATION, <<"Operation not allowed in current phase">>).
-define(MCP_MSG_NOT_INITIALIZING, <<"Server is not in initialization phase">>).
-define(MCP_MSG_ALREADY_INITIALIZED, <<"Server already initialized">>).
-define(MCP_MSG_INIT_TIMEOUT, <<"Initialization timeout exceeded">>).

%%% Phase type for type specs
-type mcp_server_phase() :: initialization | initialized | disconnected | closed.
-type mcp_client_phase() :: pre_initialization | initializing | initialized | error | closed.

%%% MCP Protocol Methods - defined in MCP specification
%%% These constants should be added to erlmcp.hrl

%%% Core Protocol Methods
-define(MCP_METHOD_INITIALIZE, <<"initialize">>).
-define(MCP_METHOD_INITIALIZED, <<"notifications/initialized">>).

%%% Resource Methods
-define(MCP_METHOD_RESOURCES_LIST, <<"resources/list">>).
-define(MCP_METHOD_RESOURCES_READ, <<"resources/read">>).
-define(MCP_METHOD_RESOURCES_TEMPLATES_LIST, <<"resources/templates/list">>).
-define(MCP_METHOD_RESOURCES_SUBSCRIBE, <<"resources/subscribe">>).
-define(MCP_METHOD_RESOURCES_UNSUBSCRIBE, <<"resources/unsubscribe">>).

%%% Tool Methods
-define(MCP_METHOD_TOOLS_LIST, <<"tools/list">>).
-define(MCP_METHOD_TOOLS_CALL, <<"tools/call">>).

%% Task Methods (NEW)
-define(MCP_METHOD_TASKS_CREATE, <<"tasks/create">>).
-define(MCP_METHOD_TASKS_LIST, <<"tasks/list">>).
-define(MCP_METHOD_TASKS_GET, <<"tasks/get">>).
-define(MCP_METHOD_TASKS_RESULT, <<"tasks/result">>).
-define(MCP_METHOD_TASKS_CANCEL, <<"tasks/cancel">>).

%%% Cancellation Methods (TASK #142: Request Cancellation)
-define(MCP_METHOD_REQUESTS_CANCEL, <<"requests/cancel">>).
-define(MCP_METHOD_NOTIFICATIONS_CANCELLED, <<"notifications/cancelled">>).

%%% Completion Methods (MCP 2025-11-25)
-define(MCP_METHOD_COMPLETION_COMPLETE, <<"completion/complete">>).

%%% Elicitation Methods (MCP 2025-11-25)
-define(MCP_METHOD_ELICITATION_CREATE, <<"elicitation/create">>).
-define(MCP_METHOD_NOTIFICATIONS_ELICITATION_COMPLETE, <<"notifications/elicitation/complete">>).

%%% Message Methods (MCP 2025-11-25)
-define(MCP_METHOD_NOTIFICATIONS_MESSAGE, <<"notifications/message">>).

%%% Ping Method (MCP 2025-11-25)
-define(MCP_METHOD_PING, <<"ping">>).

%%% Prompt Methods
-define(MCP_METHOD_PROMPTS_LIST, <<"prompts/list">>).
-define(MCP_METHOD_PROMPTS_GET, <<"prompts/get">>).

%%% Logging Methods (Gap #21)
-define(MCP_METHOD_LOGGING_SET_LEVEL, <<"logging/setLevel">>).

%%% Roots Methods (client-side capability)
-define(MCP_METHOD_ROOTS_LIST, <<"roots/list">>).

%%% Sampling Methods (for AI model sampling)
-define(MCP_METHOD_SAMPLING_CREATE_MESSAGE, <<"sampling/createMessage">>).

%%% Sampling Strategies (Gap #39: Sampling Strategy Validation)
-define(MCP_SAMPLING_STRATEGY_DETERMINISTIC, <<"deterministic">>).
-define(MCP_SAMPLING_STRATEGY_UNIFORM, <<"uniform">>).

%%% Valid sampling strategies list
-define(MCP_VALID_SAMPLING_STRATEGIES, [
    ?MCP_SAMPLING_STRATEGY_DETERMINISTIC,
    ?MCP_SAMPLING_STRATEGY_UNIFORM
]).

%%% Sampling error messages
-define(MCP_MSG_INVALID_SAMPLING_STRATEGY, <<"Invalid sampling strategy">>).
-define(MCP_PARAM_STRATEGY, <<"strategy">>).

%%% Notification Methods
-define(MCP_METHOD_NOTIFICATIONS_PROGRESS, <<"notifications/progress">>).
-define(MCP_METHOD_NOTIFICATIONS_RESOURCES_UPDATED, <<"resources/updated">>).
-define(MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED, <<"resources/list_changed">>).
-define(MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED, <<"prompts/list_changed">>).
-define(MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED, <<"tools/list_changed">>).
-define(MCP_METHOD_NOTIFICATIONS_ROOTS_LIST_CHANGED, <<"roots/list_changed">>).
-define(MCP_METHOD_NOTIFICATIONS_TASKS_STATUS, <<"notifications/tasks/status">>).

%%% Capability Names - used in capability negotiation
-define(MCP_CAPABILITY_RESOURCES, <<"resources">>).
-define(MCP_CAPABILITY_TOOLS, <<"tools">>).
-define(MCP_CAPABILITY_PROMPTS, <<"prompts">>).
-define(MCP_CAPABILITY_LOGGING, <<"logging">>).
-define(MCP_CAPABILITY_ROOTS, <<"roots">>).
-define(MCP_CAPABILITY_SAMPLING, <<"sampling">>).
-define(MCP_CAPABILITY_COMPLETIONS, <<"completions">>).  %% MCP 2025-11-25
-define(MCP_CAPABILITY_ELICITATION, <<"elicitation">>).  %% MCP 2025-11-25
-define(MCP_CAPABILITY_TASKS, <<"tasks">>).  %% MCP 2025-11-25

%%% Capability Features - used in capability negotiation
-define(MCP_FEATURE_SUBSCRIBE, <<"subscribe">>).
-define(MCP_FEATURE_LIST_CHANGED, <<"listChanged">>).

%%% Content Types
-define(MCP_CONTENT_TYPE_TEXT, <<"text">>).
-define(MCP_CONTENT_TYPE_IMAGE, <<"image">>).
-define(MCP_CONTENT_TYPE_AUDIO, <<"audio">>).
-define(MCP_CONTENT_TYPE_RESOURCE, <<"resource">>).
-define(MCP_CONTENT_TYPE_RESOURCE_LINK, <<"resource/link">>).

%%% Resource Link Fields (Gap #33: Resource Link Content Type)
-define(MCP_RESOURCE_LINK_FIELD_TYPE, <<"type">>).
-define(MCP_RESOURCE_LINK_FIELD_URI, <<"uri">>).
-define(MCP_RESOURCE_LINK_FIELD_NAME, <<"name">>).
-define(MCP_RESOURCE_LINK_FIELD_MIME_TYPE, <<"mimeType">>).
-define(MCP_RESOURCE_LINK_FIELD_SIZE, <<"size">>).

%%% Message Roles (for prompts)
-define(MCP_ROLE_USER, <<"user">>).
-define(MCP_ROLE_ASSISTANT, <<"assistant">>).
-define(MCP_ROLE_SYSTEM, <<"system">>).

%%% MIME Types - common ones used in MCP
-define(MCP_MIME_TEXT_PLAIN, <<"text/plain">>).
-define(MCP_MIME_APPLICATION_JSON, <<"application/json">>).
-define(MCP_MIME_TEXT_MARKDOWN, <<"text/markdown">>).

%%% Audio MIME Types (Gap #34: Audio Content Type Support)
-define(MCP_MIME_AUDIO_WAV, <<"audio/wav">>).
-define(MCP_MIME_AUDIO_MPEG, <<"audio/mpeg">>).
-define(MCP_MIME_AUDIO_MP3, <<"audio/mp3">>).
-define(MCP_MIME_AUDIO_AAC, <<"audio/aac">>).
-define(MCP_MIME_AUDIO_FLAC, <<"audio/flac">>).
-define(MCP_MIME_AUDIO_OGG, <<"audio/ogg">>).
-define(MCP_MIME_AUDIO_WEBM, <<"audio/webm">>).
-define(MCP_MIME_AUDIO_OPUS, <<"audio/opus">>).

%%% Audio metadata field names (Gap #34: Audio metadata support)
-define(MCP_PARAM_AUDIO_DURATION, <<"duration">>).
-define(MCP_PARAM_AUDIO_SAMPLE_RATE, <<"sampleRate">>).
-define(MCP_PARAM_AUDIO_CHANNELS, <<"channels">>).
-define(MCP_PARAM_AUDIO_BITRATE, <<"bitrate">>).

%%% Message Size Limits (Gap #45: Message Size Limits)
%%% MCP 2025-11-25 Specification Compliance:
%%% - Default message size limit: 16 MB (16777216 bytes)
%%% - Configurable per transport via sys.config
%%% - HTTP POST body limit separate from default
%%% - Oversized messages return error response (JSON-RPC) or HTTP 413
-define(MCP_DEFAULT_MESSAGE_SIZE_LIMIT, 16777216).  %% 16 MB
-define(MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT, 16777216).  %% 16 MB
-define(MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT, 16777216).  %% 16 MB
-define(MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT, 16777216).  %% 16 MB
-define(MCP_MIN_MESSAGE_SIZE_LIMIT, 1024).  %% 1 KB minimum
-define(MCP_MAX_CONFIGURABLE_SIZE_LIMIT, 104857600).  %% 100 MB maximum

%%% Logging levels (Gap #21: Log Level Enforcement)
-define(MCP_VALID_LOG_LEVELS, [debug, info, notice, warning, error, critical, alert, emergency]).
-define(MCP_DEFAULT_LOG_LEVEL, info).

%% Log level type for validation (Task #137: Logging Capability)
-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.

%%% Server/Client Info Fields
-define(MCP_INFO_NAME, <<"name">>).
-define(MCP_INFO_VERSION, <<"version">>).
-define(MCP_FIELD_PROTOCOL_VERSION, <<"protocolVersion">>).
-define(MCP_FIELD_CAPABILITIES, <<"capabilities">>).
-define(MCP_FIELD_CLIENT_INFO, <<"clientInfo">>).
-define(MCP_FIELD_SERVER_INFO, <<"serverInfo">>).

%%% Common Parameter Names
-define(MCP_PARAM_URI, <<"uri">>).
-define(MCP_PARAM_NAME, <<"name">>).
-define(MCP_PARAM_ARGUMENTS, <<"arguments">>).
-define(MCP_PARAM_DESCRIPTION, <<"description">>).
-define(MCP_PARAM_MIME_TYPE, <<"mimeType">>).
-define(MCP_PARAM_METADATA, <<"metadata">>).
-define(MCP_PARAM_CONTENT, <<"content">>).
-define(MCP_PARAM_CONTENTS, <<"contents">>).
-define(MCP_PARAM_TEXT, <<"text">>).
-define(MCP_PARAM_DATA, <<"data">>).
-define(MCP_PARAM_TYPE, <<"type">>).
-define(MCP_PARAM_ROLE, <<"role">>).
-define(MCP_PARAM_MESSAGES, <<"messages">>).
-define(MCP_PARAM_RESOURCES, <<"resources">>).
-define(MCP_PARAM_RESOURCE_TEMPLATES, <<"resourceTemplates">>).
-define(MCP_PARAM_TOOLS, <<"tools">>).
-define(MCP_PARAM_PROMPTS, <<"prompts">>).
-define(MCP_PARAM_ROOTS, <<"roots">>).
-define(MCP_PARAM_INPUT_SCHEMA, <<"inputSchema">>).
-define(MCP_PARAM_URI_TEMPLATE, <<"uriTemplate">>).
-define(MCP_PARAM_REQUIRED, <<"required">>).
-define(MCP_PARAM_PROGRESS_TOKEN, <<"progressToken">>).
-define(MCP_PARAM_PROGRESS, <<"progress">>).
-define(MCP_PARAM_TOTAL, <<"total">>).
-define(MCP_PARAM_TASK_ID, <<"taskId">>).
-define(MCP_PARAM_TASK, <<"task">>).
-define(MCP_PARAM_TASKS, <<"tasks">>).
-define(MCP_PARAM_STATUS, <<"status">>).
-define(MCP_PARAM_RESULT, <<"result">>).
-define(MCP_PARAM_ERROR, <<"error">>).
-define(MCP_PARAM_ANNOTATIONS, <<"annotations">>).
-define(MCP_PARAM_REQUEST_ID, <<"requestId">>).
-define(MCP_PARAM_REASON, <<"reason">>).
-define(MCP_PARAM_TIMESTAMP, <<"timestamp">>).
-define(MCP_PARAM_CURSOR, <<"cursor">>).  %% Pagination cursor (MCP 2025-11-25)
-define(MCP_PARAM_INCLUDE_CONTEXT, <<"includeContext">>).  %% Sampling include context (MCP 2025-11-25)
-define(MCP_PARAM_TOOL_CHOICE, <<"toolChoice">>).  %% Sampling tool choice (MCP 2025-11-25)
-define(MCP_PARAM_META, <<"_meta">>).  %% Metadata field (MCP 2025-11-25)
-define(MCP_PARAM_INSTRUCTIONS, <<"instructions">>).  %% Server instructions (MCP 2025-11-25)
-define(MCP_PARAM_ELICITATIONS, <<"elicitations">>).  %% URL elicitation (MCP 2025-11-25)

%%% JSON-RPC Types
-type json_rpc_id() :: null | binary() | integer().
-type json_rpc_params() :: map() | list() | undefined.

%%% JSON-RPC Records
-record(json_rpc_request, {
    id :: json_rpc_id(),
    method :: binary(),
    params :: json_rpc_params()
}).

-record(json_rpc_response, {
    id :: json_rpc_id(),
    result :: term() | undefined,
    error :: map() | undefined
}).

-record(json_rpc_notification, {
    method :: binary(),
    params :: json_rpc_params()
}).

%%% MCP Capability Records
%% Base capability with enabled flag
-record(mcp_capability, {
    enabled = false :: boolean()
}).

%% Feature flags for resources capability
-record(mcp_resources_capability, {
    subscribe = false :: boolean(),
    listChanged = false :: boolean()
}).

%% Feature flags for tools capability
-record(mcp_tools_capability, {
    listChanged = false :: boolean()
}).

%% Feature flags for prompts capability
-record(mcp_prompts_capability, {
    listChanged = false :: boolean()
}).

%% Logging capability (no specific features)
-record(mcp_logging_capability, {
}).

%% Sampling capability with model preferences
-record(mcp_sampling_capability, {
    modelPreferences = undefined :: map() | undefined
}).

%% Roots capability
-record(mcp_roots_capability, {
}).

%% Client capabilities sent during initialize
-record(mcp_client_capabilities, {
    roots = #mcp_capability{} :: #mcp_capability{},
    sampling = #mcp_sampling_capability{} :: #mcp_sampling_capability{},
    tools = #mcp_tools_capability{} :: #mcp_tools_capability{},
    experimental = undefined :: map() | undefined
}).

%% Server capabilities sent in initialize response
-record(mcp_server_capabilities, {
    resources = #mcp_resources_capability{} :: #mcp_resources_capability{},
    tools = #mcp_tools_capability{} :: #mcp_tools_capability{},
    prompts = #mcp_prompts_capability{} :: #mcp_prompts_capability{},
    logging = #mcp_logging_capability{} :: #mcp_logging_capability{},
    sampling = #mcp_sampling_capability{} :: #mcp_sampling_capability{},
    roots = #mcp_roots_capability{} :: #mcp_roots_capability{},
    experimental = undefined :: map() | undefined
}).

%%% MCP Content Records
%%% MCP Annotation Record (Gap #22: Annotations Support)
%% Annotations are optional metadata about content blocks
%% Example: {name = <<"audience">>, value = <<"user">>}
-record(mcp_annotation, {
    name :: binary(),
    value :: binary() | number() | boolean() | map() | undefined
}).

%%% Gap #33: Resource Link Record (defined before mcp_content)
%%% Represents a link to an external resource within content blocks
-record(mcp_resource_link, {
    uri :: binary(),
    name :: binary() | undefined,
    mime_type :: binary() | undefined,
    size :: integer() | undefined
}).

-record(mcp_content, {
    type :: binary(),
    text :: binary() | undefined,
    data :: binary() | undefined,
    mime_type :: binary() | undefined,
    annotations = [] :: [#mcp_annotation{}],
    resource_link = undefined :: #mcp_resource_link{} | undefined
}).

-record(mcp_resource, {
    uri :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    mime_type :: binary() | undefined,
    metadata :: map() | undefined,
    audience :: binary() | undefined,
    priority :: integer() | undefined,
    last_modified :: integer() | undefined,
    annotations :: map() | undefined,
    size :: integer() | undefined
}).

-record(mcp_resource_template, {
    uri_template :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    mime_type :: binary() | undefined
}).

-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: map() | undefined,
    metadata :: map() | undefined,
    experimental = undefined :: map() | undefined,
    version :: binary() | undefined,
    deprecated = false :: boolean()
}).

-record(mcp_prompt_argument, {
    name :: binary(),
    description :: binary() | undefined,
    required :: boolean()
}).

-record(mcp_prompt, {
    name :: binary(),
    description :: binary() | undefined,
    arguments :: [#mcp_prompt_argument{}] | undefined,
    input_schema :: map() | undefined  % Gap #42: JSON Schema for argument validation
}).

-record(mcp_root, {
    uri :: binary(),
    name :: binary() | undefined
}).

-record(mcp_progress_token, {
    token :: binary() | integer()
}).

-record(mcp_progress_notification, {
    progress_token :: #mcp_progress_token{},
    progress :: float(),
    total :: float()
}).

-record(mcp_error, {
    code :: integer(),
    message :: binary(),
    data :: term() | undefined
}).

%%% Export types
-export_type([
    json_rpc_id/0,
    json_rpc_params/0,
    log_level/0
]).

-record(mcp_model_preferences, {
    cost_priority :: float() | undefined,
    speed_priority :: float() | undefined,
    intelligence_priority :: float() | undefined,
    temperature :: float() | undefined,
    max_tokens :: integer() | undefined,
    stop_sequences :: [binary()] | undefined
}).

%% MCP App Record (for MCP Apps feature)
-type app_id() :: binary().
-type app_name() :: binary().
-type app_version() :: binary().
-type app_status() :: initialized | running | stopped | error.
-type permission() :: atom().
-type app_state() :: map().

-record(mcp_app, {
    id :: app_id(),
    name :: app_name(),
    version :: app_version(),
    description :: binary(),
    status = initialized :: app_status(),
    uri :: binary() | undefined,
    manifest :: map() | undefined,
    permissions = sets:new() :: sets:set(permission()),
    state = #{} :: app_state(),
    created_at :: integer(),
    activated_at :: integer() | undefined,
    resources = [] :: [binary()],
    error :: binary() | undefined
}).

%% MCP Server Types and State Record
-type server_id() :: atom().
-type resource_handler() :: fun((binary()) -> binary() | #mcp_content{}).
-type tool_handler() :: fun((map()) -> binary() | #mcp_content{} | [#mcp_content{}]).
-type prompt_handler() :: fun((map()) -> binary() | [map()]).

-record(mcp_server_state, {
    server_id :: server_id(),
    phase = initialization :: mcp_server_phase(),
    init_timeout_ref :: reference() | undefined,
    init_timeout_ms = 5000 :: pos_integer(),
    capabilities :: #mcp_server_capabilities{},
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    protocol_version :: binary() | undefined,
    resources = #{} :: #{binary() => {#mcp_resource{}, resource_handler()}},
    resource_templates = #{} :: #{binary() => {#mcp_resource_template{}, resource_handler()}},
    tools = #{} :: #{binary() => {#mcp_tool{}, tool_handler(), map() | undefined}},
    prompts = #{} :: #{binary() => {#mcp_prompt{}, prompt_handler()}},
    subscriptions = #{} :: #{binary() => sets:set(pid())},
    progress_tokens = #{} :: #{binary() | integer() => #mcp_progress_notification{}},
    notifier_pid :: pid() | undefined,
    initialized = false :: boolean()
}).

%%% Trace Analysis Record (for erlmcp_trace_analyzer)
-record(trace_analysis, {
    trace_id :: binary(),
    bottlenecks = [] :: [map()],
    performance_score = 0 :: number(),
    avg_duration = 0 :: number(),
    error_rate = 0.0 :: float(),
    recommendations = [] :: [binary()],
    analyzed_at :: integer()
}).

%%% Request ID Space Monitoring (RPN 720: Request ID Overflow Fix)
-define(MAX_SAFE_REQUEST_ID, 1152921504606846975).  % 2^60 - 1
-define(ID_WARNING_THRESHOLD, 922337203685477580).  % 80% of max
-define(ID_CRITICAL_THRESHOLD, 1037629354146165277). % 90% of max
-define(ID_RESERVED_THRESHOLD, 1106804644422573050). % 96% of max

-endif.
