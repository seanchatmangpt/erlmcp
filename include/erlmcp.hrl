-ifndef(ERLMCP_HRL).
-define(ERLMCP_HRL, 1).

%%% MCP Protocol Version
-define(APP_NAME, <<"erlmcp">>).
-define(MCP_VERSION, <<"2025-06-18">>).

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

%%% MCP-Specific Error Codes (using the server error range)
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
-define(MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION, -32003).  %% Gap #30

%%% Convenience list of all valid error codes for validation
-define(VALID_ERROR_CODES, [
    -32700,  % Parse error
    -32600,  % Invalid Request
    -32601,  % Method not found
    -32602,  % Invalid params
    -32603,  % Internal error
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
    -32011,  % Tool description too long (Gap #40)
    -32012   % Message too large (Gap #45)
]).

%%% Error Messages (for consistency)
-define(JSONRPC_MSG_PARSE_ERROR, <<"Parse error">>).
-define(JSONRPC_MSG_INVALID_REQUEST, <<"Invalid Request">>).
-define(JSONRPC_MSG_METHOD_NOT_FOUND, <<"Method not found">>).
-define(JSONRPC_MSG_INVALID_PARAMS, <<"Invalid params">>).
-define(JSONRPC_MSG_INTERNAL_ERROR, <<"Internal error">>).

%%% Model Preferences Validation Messages
-define(MCP_MSG_INVALID_MODEL_PREFERENCES, <<"Invalid model preferences">>).
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

-define(MCP_MSG_RESOURCE_NOT_FOUND, <<"Resource not found">>).
-define(MCP_MSG_TOOL_NOT_FOUND, <<"Tool not found">>).
-define(MCP_MSG_PROMPT_NOT_FOUND, <<"Prompt not found">>).
-define(MCP_MSG_MISSING_URI_PARAMETER, <<"Missing uri parameter">>).
-define(MCP_MSG_MISSING_TOOL_NAME, <<"Missing tool name">>).
-define(MCP_MSG_MISSING_PROMPT_NAME, <<"Missing prompt name">>).
-define(MCP_MSG_CAPABILITY_NOT_SUPPORTED, <<"Capability not supported">>).
-define(MCP_MSG_NOT_INITIALIZED, <<"Server not initialized">>).
-define(MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION, <<"Unsupported protocol version">>).  %% Gap #30

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

%%% Prompt Methods
-define(MCP_METHOD_PROMPTS_LIST, <<"prompts/list">>).
-define(MCP_METHOD_PROMPTS_GET, <<"prompts/get">>).

%%% Logging Methods (Gap #21)
-define(MCP_METHOD_LOGGING_SET_LEVEL, <<"logging/setLevel">>).

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
-define(MCP_METHOD_NOTIFICATIONS_TASKS_STATUS, <<"notifications/tasks/status">>).

%%% Capability Names - used in capability negotiation
-define(MCP_CAPABILITY_RESOURCES, <<"resources">>).
-define(MCP_CAPABILITY_TOOLS, <<"tools">>).
-define(MCP_CAPABILITY_PROMPTS, <<"prompts">>).
-define(MCP_CAPABILITY_LOGGING, <<"logging">>).
-define(MCP_CAPABILITY_ROOTS, <<"roots">>).
-define(MCP_CAPABILITY_SAMPLING, <<"sampling">>).

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
-define(MCP_ERROR_MESSAGE_TOO_LARGE, -32012).
-define(MCP_MSG_MESSAGE_TOO_LARGE, <<"Message size exceeds maximum allowed">>).

%%% Tool Description Validation (Gap #40: Tool Description Length)
%%% MCP Specification Requirements:
%%% - Tool descriptions MUST have a maximum length
%%% - Default maximum: 1000 characters per MCP spec
%%% - Configurable via sys.config
%%% - Validation enforced on tool registration
-define(MCP_TOOL_DESCRIPTION_MAX_LENGTH_DEFAULT, 1000).
-define(MCP_TOOL_DESCRIPTION_MIN_LENGTH, 0).
-define(MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG, -32011).
-define(MCP_MSG_TOOL_DESCRIPTION_TOO_LONG, <<"Tool description exceeds maximum length">>).

%%% Logging levels (Gap #21: Log Level Enforcement)
-define(MCP_VALID_LOG_LEVELS, [debug, info, notice, warning, error, critical, alert, emergency]).
-define(MCP_DEFAULT_LOG_LEVEL, info).

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
    sampling = #mcp_capability{} :: #mcp_capability{},
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
    metadata :: map() | undefined
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
    input_schema :: map() | undefined
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
    json_rpc_params/0
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

-record(state, {
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

-endif.
