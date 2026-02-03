%%%-------------------------------------------------------------------
%%% @doc A2A (Agent-to-Agent) Protocol Header
%%%
%%% This header file defines the records, types, and constants for the
%%% Google A2A (Agent-to-Agent) protocol specification, providing
%%% interoperability between AI agents through a standardized protocol.
%%%
%%% Protocol Specification: https://github.com/google/a2a-spec
%%% Version: 0.3 (Draft)
%%%
%%% Integration with erlmcp:
%%% - Uses JSON-RPC 2.0 as base transport (same as MCP)
%%% - Task-centric model complementing MCP's tool-centric model
%%% - Supports streaming, push notifications, and agent discovery
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_A2A_HRL).
-define(ERLMCP_A2A_HRL, 1).

%%% ==================================================================
%%% A2A Protocol Version
%%% ==================================================================
-define(A2A_PROTOCOL_VERSION, <<"0.3">>).
-define(A2A_PROTOCOL_BINDING_JSONRPC, <<"JSONRPC">>).
-define(A2A_PROTOCOL_BINDING_GRPC, <<"GRPC">>).
-define(A2A_PROTOCOL_BINDING_HTTP_JSON, <<"HTTP+JSON">>).

%%% ==================================================================
%%% A2A Error Codes (using -33xxx range to avoid MCP collision)
%%% ==================================================================

%% Core A2A errors (-33001 to -33010)
-define(A2A_ERROR_TASK_NOT_FOUND, -33001).
-define(A2A_ERROR_CONTEXT_NOT_FOUND, -33002).
-define(A2A_ERROR_ARTIFACT_NOT_FOUND, -33003).
-define(A2A_ERROR_MESSAGE_INVALID, -33004).
-define(A2A_ERROR_AGENT_NOT_FOUND, -33005).
-define(A2A_ERROR_CAPABILITY_NOT_SUPPORTED, -33006).
-define(A2A_ERROR_PUSH_NOTIFICATION_FAILED, -33007).
-define(A2A_ERROR_SUBSCRIPTION_NOT_FOUND, -33008).
-define(A2A_ERROR_SKILL_NOT_FOUND, -33009).
-define(A2A_ERROR_EXTENSION_NOT_SUPPORTED, -33010).

%% Task state errors (-33011 to -33020)
-define(A2A_ERROR_TASK_ALREADY_TERMINAL, -33011).
-define(A2A_ERROR_TASK_NOT_CANCELABLE, -33012).
-define(A2A_ERROR_TASK_STATE_TRANSITION_INVALID, -33013).
-define(A2A_ERROR_TASK_WORKING, -33014).
-define(A2A_ERROR_TASK_SUBMITTED, -33015).
-define(A2A_ERROR_TASK_INPUT_REQUIRED, -33016).
-define(A2A_ERROR_TASK_AUTH_REQUIRED, -33017).
-define(A2A_ERROR_TASK_REJECTED, -33018).
-define(A2A_ERROR_TASK_FAILED, -33019).
-define(A2A_ERROR_TASK_COMPLETED, -33020).

%% Message and Part errors (-33021 to -33030)
-define(A2A_ERROR_PART_INVALID, -33021).
-define(A2A_ERROR_PART_TOO_LARGE, -33022).
-define(A2A_ERROR_MEDIA_TYPE_UNSUPPORTED, -33023).
-define(A2A_ERROR_FILE_NOT_FOUND, -33024).
-define(A2A_ERROR_URL_INVALID, -33025).
-define(A2A_ERROR_DATA_INVALID, -33026).
-define(A2A_ERROR_MESSAGE_TOO_LARGE, -33027).
-define(A2A_ERROR_ROLE_INVALID, -33028).
-define(A2A_ERROR_CONTEXT_ID_MISMATCH, -33029).
-define(A2A_ERROR_TASK_ID_MISMATCH, -33030).

%% Agent Card errors (-33031 to -33040)
-define(A2A_ERROR_AGENT_CARD_INVALID, -33031).
-define(A2A_ERROR_AGENT_CARD_SIGNATURE_INVALID, -33032).
-define(A2A_ERROR_INTERFACE_NOT_FOUND, -33033).
-define(A2A_ERROR_PROVIDER_INVALID, -33034).
-define(A2A_ERROR_SKILL_INVALID, -33035).
-define(A2A_ERROR_SECURITY_SCHEME_INVALID, -33036).
-define(A2A_ERROR_SECURITY_REQUIREMENT_NOT_MET, -33037).
-define(A2A_ERROR_EXTENDED_CARD_NOT_AVAILABLE, -33038).
-define(A2A_ERROR_INPUT_MODE_UNSUPPORTED, -33039).
-define(A2A_ERROR_OUTPUT_MODE_UNSUPPORTED, -33040).

%% Push notification errors (-33041 to -33050)
-define(A2A_ERROR_PUSH_CONFIG_NOT_FOUND, -33041).
-define(A2A_ERROR_PUSH_CONFIG_INVALID, -33042).
-define(A2A_ERROR_PUSH_URL_INVALID, -33043).
-define(A2A_ERROR_PUSH_AUTH_FAILED, -33044).
-define(A2A_ERROR_PUSH_TOKEN_INVALID, -33045).
-define(A2A_ERROR_PUSH_DELIVERY_FAILED, -33046).
-define(A2A_ERROR_PUSH_LIMIT_EXCEEDED, -33047).
-define(A2A_ERROR_PUSH_DISABLED, -33048).
-define(A2A_ERROR_WEBHOOK_TIMEOUT, -33049).
-define(A2A_ERROR_WEBHOOK_ERROR, -33050).

%% Streaming errors (-33051 to -33060)
-define(A2A_ERROR_STREAMING_NOT_SUPPORTED, -33051).
-define(A2A_ERROR_STREAM_CLOSED, -33052).
-define(A2A_ERROR_STREAM_ERROR, -33053).
-define(A2A_ERROR_STREAM_TIMEOUT, -33054).
-define(A2A_ERROR_STREAM_CANCELLED, -33055).
-define(A2A_ERROR_ARTIFACT_APPEND_INVALID, -33056).
-define(A2A_ERROR_CHUNK_OUT_OF_ORDER, -33057).
-define(A2A_ERROR_CHUNK_DUPLICATE, -33058).
-define(A2A_ERROR_STREAM_ALREADY_ACTIVE, -33059).
-define(A2A_ERROR_SUBSCRIBE_FAILED, -33060).

%% Pagination errors (-33061 to -33070)
-define(A2A_ERROR_PAGE_TOKEN_INVALID, -33061).
-define(A2A_ERROR_PAGE_SIZE_INVALID, -33062).
-define(A2A_ERROR_PAGE_SIZE_TOO_LARGE, -33063).
-define(A2A_ERROR_FILTER_INVALID, -33064).
-define(A2A_ERROR_SORT_INVALID, -33065).
-define(A2A_ERROR_TIMESTAMP_INVALID, -33066).
-define(A2A_ERROR_HISTORY_LENGTH_INVALID, -33067).
-define(A2A_ERROR_OFFSET_INVALID, -33068).
-define(A2A_ERROR_LIMIT_EXCEEDED, -33069).
-define(A2A_ERROR_QUERY_TOO_COMPLEX, -33070).

%%% ==================================================================
%%% A2A Error Messages
%%% ==================================================================

%% Core errors
-define(A2A_MSG_TASK_NOT_FOUND, <<"Task not found">>).
-define(A2A_MSG_CONTEXT_NOT_FOUND, <<"Context not found">>).
-define(A2A_MSG_ARTIFACT_NOT_FOUND, <<"Artifact not found">>).
-define(A2A_MSG_MESSAGE_INVALID, <<"Invalid message">>).
-define(A2A_MSG_AGENT_NOT_FOUND, <<"Agent not found">>).
-define(A2A_MSG_CAPABILITY_NOT_SUPPORTED, <<"Capability not supported">>).
-define(A2A_MSG_PUSH_NOTIFICATION_FAILED, <<"Push notification failed">>).
-define(A2A_MSG_SUBSCRIPTION_NOT_FOUND, <<"Subscription not found">>).
-define(A2A_MSG_SKILL_NOT_FOUND, <<"Skill not found">>).
-define(A2A_MSG_EXTENSION_NOT_SUPPORTED, <<"Extension not supported">>).

%% Task state errors
-define(A2A_MSG_TASK_ALREADY_TERMINAL, <<"Task is already in terminal state">>).
-define(A2A_MSG_TASK_NOT_CANCELABLE, <<"Task cannot be canceled in current state">>).
-define(A2A_MSG_TASK_STATE_TRANSITION_INVALID, <<"Invalid task state transition">>).

%%% ==================================================================
%%% A2A Methods (JSON-RPC method names)
%%% ==================================================================

%% Core message operations
-define(A2A_METHOD_SEND_MESSAGE, <<"a2a/sendMessage">>).
-define(A2A_METHOD_SEND_STREAMING_MESSAGE, <<"a2a/sendStreamingMessage">>).

%% Task operations
-define(A2A_METHOD_GET_TASK, <<"a2a/getTask">>).
-define(A2A_METHOD_LIST_TASKS, <<"a2a/listTasks">>).
-define(A2A_METHOD_CANCEL_TASK, <<"a2a/cancelTask">>).
-define(A2A_METHOD_SUBSCRIBE_TO_TASK, <<"a2a/subscribeToTask">>).

%% Push notification operations
-define(A2A_METHOD_CREATE_PUSH_CONFIG, <<"a2a/createTaskPushNotificationConfig">>).
-define(A2A_METHOD_GET_PUSH_CONFIG, <<"a2a/getTaskPushNotificationConfig">>).
-define(A2A_METHOD_LIST_PUSH_CONFIGS, <<"a2a/listTaskPushNotificationConfigs">>).
-define(A2A_METHOD_DELETE_PUSH_CONFIG, <<"a2a/deleteTaskPushNotificationConfig">>).

%% Agent card operations
-define(A2A_METHOD_GET_EXTENDED_AGENT_CARD, <<"a2a/getExtendedAgentCard">>).

%% Notification methods (server -> client)
-define(A2A_NOTIFICATION_TASK_STATUS_UPDATE, <<"a2a/taskStatusUpdate">>).
-define(A2A_NOTIFICATION_TASK_ARTIFACT_UPDATE, <<"a2a/taskArtifactUpdate">>).

%%% ==================================================================
%%% A2A Task States
%%% ==================================================================

-define(A2A_TASK_STATE_UNSPECIFIED, unspecified).
-define(A2A_TASK_STATE_SUBMITTED, submitted).
-define(A2A_TASK_STATE_WORKING, working).
-define(A2A_TASK_STATE_COMPLETED, completed).
-define(A2A_TASK_STATE_FAILED, failed).
-define(A2A_TASK_STATE_CANCELED, canceled).
-define(A2A_TASK_STATE_INPUT_REQUIRED, input_required).
-define(A2A_TASK_STATE_REJECTED, rejected).
-define(A2A_TASK_STATE_AUTH_REQUIRED, auth_required).

%% Terminal states (task cannot transition further)
-define(A2A_TERMINAL_STATES, [completed, failed, canceled, rejected]).

%% Interrupted states (task waiting for external input)
-define(A2A_INTERRUPTED_STATES, [input_required, auth_required]).

%% Active states (task is being processed)
-define(A2A_ACTIVE_STATES, [submitted, working]).

%%% ==================================================================
%%% A2A Message Roles
%%% ==================================================================

-define(A2A_ROLE_UNSPECIFIED, unspecified).
-define(A2A_ROLE_USER, user).
-define(A2A_ROLE_AGENT, agent).

%%% ==================================================================
%%% A2A Content Types (MIME types)
%%% ==================================================================

-define(A2A_MIME_TEXT_PLAIN, <<"text/plain">>).
-define(A2A_MIME_APPLICATION_JSON, <<"application/json">>).
-define(A2A_MIME_IMAGE_PNG, <<"image/png">>).
-define(A2A_MIME_IMAGE_JPEG, <<"image/jpeg">>).
-define(A2A_MIME_IMAGE_GIF, <<"image/gif">>).
-define(A2A_MIME_IMAGE_WEBP, <<"image/webp">>).
-define(A2A_MIME_VIDEO_MP4, <<"video/mp4">>).
-define(A2A_MIME_VIDEO_WEBM, <<"video/webm">>).
-define(A2A_MIME_AUDIO_MPEG, <<"audio/mpeg">>).
-define(A2A_MIME_AUDIO_WAV, <<"audio/wav">>).
-define(A2A_MIME_APPLICATION_PDF, <<"application/pdf">>).
-define(A2A_MIME_APPLICATION_OCTET_STREAM, <<"application/octet-stream">>).

%%% ==================================================================
%%% A2A Pagination Defaults
%%% ==================================================================

-define(A2A_DEFAULT_PAGE_SIZE, 50).
-define(A2A_MAX_PAGE_SIZE, 100).
-define(A2A_MIN_PAGE_SIZE, 1).

%%% ==================================================================
%%% A2A Types
%%% ==================================================================

%% Task state type
-type a2a_task_state() :: unspecified | submitted | working | completed |
                          failed | canceled | input_required | rejected | auth_required.

%% Role type
-type a2a_role() :: unspecified | user | agent.

%% Part content type discriminator
-type a2a_part_content_type() :: text | raw | url | data.

%% Security scheme type discriminator
-type a2a_security_scheme_type() :: api_key | http_auth | oauth2 | openid_connect | mtls.

%% OAuth flow type
-type a2a_oauth_flow_type() :: authorization_code | client_credentials | device_code.

%%% ==================================================================
%%% A2A Part Record
%%% Represents a section of communication content
%%% ==================================================================

-record(a2a_part, {
    %% Content - only one should be set (discriminated union)
    text :: binary() | undefined,
    raw :: binary() | undefined,            % Base64 encoded in JSON
    url :: binary() | undefined,
    data :: map() | list() | binary() | number() | boolean() | undefined,  % Arbitrary JSON value

    %% Metadata
    metadata :: map() | undefined,
    filename :: binary() | undefined,
    media_type :: binary() | undefined      % MIME type
}).

%%% ==================================================================
%%% A2A Message Record
%%% One unit of communication between client and server
%%% ==================================================================

-record(a2a_message, {
    message_id :: binary(),                 % Required, UUID
    context_id :: binary() | undefined,     % Optional, associates with context
    task_id :: binary() | undefined,        % Optional, associates with task
    role :: a2a_role(),                     % Required, user or agent
    parts :: [#a2a_part{}],                 % Required, at least one part
    metadata :: map() | undefined,
    extensions :: [binary()] | undefined,   % URIs of extensions
    reference_task_ids :: [binary()] | undefined  % Task IDs for additional context
}).

%%% ==================================================================
%%% A2A Artifact Record
%%% Represents task outputs
%%% ==================================================================

-record(a2a_artifact, {
    artifact_id :: binary(),                % Required, unique within task
    name :: binary() | undefined,
    description :: binary() | undefined,
    parts :: [#a2a_part{}],                 % Required, at least one part
    metadata :: map() | undefined,
    extensions :: [binary()] | undefined    % URIs of extensions
}).

%%% ==================================================================
%%% A2A Task Status Record
%%% Container for task status
%%% ==================================================================

-record(a2a_task_status, {
    state :: a2a_task_state(),              % Required
    message :: #a2a_message{} | undefined,  % Optional status message
    timestamp :: binary() | undefined       % ISO 8601 timestamp
}).

%%% ==================================================================
%%% A2A Task Record
%%% Core unit of action for A2A
%%% ==================================================================

-record(a2a_task, {
    id :: binary(),                         % Required, UUID
    context_id :: binary(),                 % Required, UUID
    status :: #a2a_task_status{},           % Required
    artifacts :: [#a2a_artifact{}] | undefined,
    history :: [#a2a_message{}] | undefined,
    metadata :: map() | undefined
}).

%%% ==================================================================
%%% A2A Push Notification Config
%%% ==================================================================

-record(a2a_authentication_info, {
    scheme :: binary(),                     % Required, e.g., "Bearer", "Basic"
    credentials :: binary() | undefined
}).

-record(a2a_push_notification_config, {
    id :: binary() | undefined,             % UUID
    url :: binary(),                        % Required, notification URL
    token :: binary() | undefined,          % Session/task-specific token
    authentication :: #a2a_authentication_info{} | undefined
}).

-record(a2a_task_push_notification_config, {
    tenant :: binary() | undefined,
    id :: binary(),                         % Required
    task_id :: binary(),                    % Required
    push_notification_config :: #a2a_push_notification_config{}  % Required
}).

%%% ==================================================================
%%% A2A Task Events (for streaming)
%%% ==================================================================

-record(a2a_task_status_update_event, {
    task_id :: binary(),                    % Required
    context_id :: binary(),                 % Required
    status :: #a2a_task_status{},           % Required
    metadata :: map() | undefined
}).

-record(a2a_task_artifact_update_event, {
    task_id :: binary(),                    % Required
    context_id :: binary(),                 % Required
    artifact :: #a2a_artifact{},            % Required
    append :: boolean(),                    % If true, append to previous artifact
    last_chunk :: boolean(),                % If true, this is the final chunk
    metadata :: map() | undefined
}).

%%% ==================================================================
%%% A2A Agent Card Records
%%% ==================================================================

%% Agent Interface - declares URL, transport and protocol version
-record(a2a_agent_interface, {
    url :: binary(),                        % Required, absolute HTTPS URL
    protocol_binding :: binary(),           % Required, e.g., "JSONRPC", "GRPC", "HTTP+JSON"
    tenant :: binary() | undefined,
    protocol_version :: binary()            % Required, e.g., "0.3"
}).

%% Agent Provider
-record(a2a_agent_provider, {
    url :: binary(),                        % Required
    organization :: binary()                % Required
}).

%% Agent Extension
-record(a2a_agent_extension, {
    uri :: binary(),                        % Unique URI identifying the extension
    description :: binary() | undefined,
    required :: boolean(),                  % Client must understand if true
    params :: map() | undefined             % Extension-specific config
}).

%% Agent Capabilities
-record(a2a_agent_capabilities, {
    streaming :: boolean() | undefined,
    push_notifications :: boolean() | undefined,
    extensions :: [#a2a_agent_extension{}] | undefined,
    extended_agent_card :: boolean() | undefined
}).

%% Agent Skill
-record(a2a_agent_skill, {
    id :: binary(),                         % Required, unique identifier
    name :: binary(),                       % Required, human-readable
    description :: binary(),                % Required
    tags :: [binary()],                     % Required, keywords
    examples :: [binary()] | undefined,
    input_modes :: [binary()] | undefined,  % Overrides agent defaults
    output_modes :: [binary()] | undefined, % Overrides agent defaults
    security_requirements :: [map()] | undefined
}).

%% Agent Card Signature (JWS)
-record(a2a_agent_card_signature, {
    protected :: binary(),                  % Required, base64url-encoded JSON
    signature :: binary(),                  % Required, base64url-encoded
    header :: map() | undefined             % Unprotected header
}).

%%% ==================================================================
%%% A2A Security Schemes
%%% ==================================================================

-record(a2a_api_key_security_scheme, {
    description :: binary() | undefined,
    location :: binary(),                   % Required: "query", "header", "cookie"
    name :: binary()                        % Required: parameter name
}).

-record(a2a_http_auth_security_scheme, {
    description :: binary() | undefined,
    scheme :: binary(),                     % Required: e.g., "Bearer"
    bearer_format :: binary() | undefined   % e.g., "JWT"
}).

-record(a2a_authorization_code_oauth_flow, {
    authorization_url :: binary(),          % Required
    token_url :: binary(),                  % Required
    refresh_url :: binary() | undefined,
    scopes :: #{binary() => binary()},      % Required
    pkce_required :: boolean() | undefined
}).

-record(a2a_client_credentials_oauth_flow, {
    token_url :: binary(),                  % Required
    refresh_url :: binary() | undefined,
    scopes :: #{binary() => binary()}       % Required
}).

-record(a2a_device_code_oauth_flow, {
    device_authorization_url :: binary(),   % Required
    token_url :: binary(),                  % Required
    refresh_url :: binary() | undefined,
    scopes :: #{binary() => binary()}       % Required
}).

-record(a2a_oauth_flows, {
    %% Only one should be set
    authorization_code :: #a2a_authorization_code_oauth_flow{} | undefined,
    client_credentials :: #a2a_client_credentials_oauth_flow{} | undefined,
    device_code :: #a2a_device_code_oauth_flow{} | undefined
}).

-record(a2a_oauth2_security_scheme, {
    description :: binary() | undefined,
    flows :: #a2a_oauth_flows{},            % Required
    oauth2_metadata_url :: binary() | undefined
}).

-record(a2a_openid_connect_security_scheme, {
    description :: binary() | undefined,
    open_id_connect_url :: binary()         % Required
}).

-record(a2a_mtls_security_scheme, {
    description :: binary() | undefined
}).

%% Security Scheme union wrapper
-record(a2a_security_scheme, {
    %% Only one should be set (discriminated union)
    api_key :: #a2a_api_key_security_scheme{} | undefined,
    http_auth :: #a2a_http_auth_security_scheme{} | undefined,
    oauth2 :: #a2a_oauth2_security_scheme{} | undefined,
    openid_connect :: #a2a_openid_connect_security_scheme{} | undefined,
    mtls :: #a2a_mtls_security_scheme{} | undefined
}).

%%% ==================================================================
%%% A2A Agent Card (main agent discovery record)
%%% ==================================================================

-record(a2a_agent_card, {
    name :: binary(),                       % Required
    description :: binary(),                % Required
    supported_interfaces :: [#a2a_agent_interface{}],  % Required, ordered list
    provider :: #a2a_agent_provider{} | undefined,
    version :: binary(),                    % Required
    documentation_url :: binary() | undefined,
    capabilities :: #a2a_agent_capabilities{},  % Required
    security_schemes :: #{binary() => #a2a_security_scheme{}} | undefined,
    security_requirements :: [map()] | undefined,
    default_input_modes :: [binary()],      % Required, MIME types
    default_output_modes :: [binary()],     % Required, MIME types
    skills :: [#a2a_agent_skill{}],         % Required
    signatures :: [#a2a_agent_card_signature{}] | undefined,
    icon_url :: binary() | undefined
}).

%%% ==================================================================
%%% A2A Request Records
%%% ==================================================================

-record(a2a_send_message_configuration, {
    accepted_output_modes :: [binary()] | undefined,
    push_notification_config :: #a2a_push_notification_config{} | undefined,
    history_length :: integer() | undefined,
    blocking :: boolean()                   % Default false
}).

-record(a2a_send_message_request, {
    tenant :: binary() | undefined,
    message :: #a2a_message{},              % Required
    configuration :: #a2a_send_message_configuration{} | undefined,
    metadata :: map() | undefined
}).

-record(a2a_get_task_request, {
    tenant :: binary() | undefined,
    id :: binary(),                         % Required
    history_length :: integer() | undefined
}).

-record(a2a_list_tasks_request, {
    tenant :: binary() | undefined,
    context_id :: binary() | undefined,
    status :: a2a_task_state() | undefined,
    page_size :: integer() | undefined,
    page_token :: binary() | undefined,
    history_length :: integer() | undefined,
    status_timestamp_after :: binary() | undefined,  % ISO 8601
    include_artifacts :: boolean() | undefined
}).

-record(a2a_cancel_task_request, {
    tenant :: binary() | undefined,
    id :: binary()                          % Required
}).

-record(a2a_subscribe_to_task_request, {
    tenant :: binary() | undefined,
    id :: binary()                          % Required
}).

-record(a2a_create_push_config_request, {
    tenant :: binary() | undefined,
    task_id :: binary(),                    % Required
    config_id :: binary(),                  % Required
    config :: #a2a_push_notification_config{}  % Required
}).

-record(a2a_get_push_config_request, {
    tenant :: binary() | undefined,
    task_id :: binary(),                    % Required
    id :: binary()                          % Required
}).

-record(a2a_list_push_configs_request, {
    tenant :: binary() | undefined,
    task_id :: binary(),                    % Required
    page_size :: integer() | undefined,
    page_token :: binary() | undefined
}).

-record(a2a_delete_push_config_request, {
    tenant :: binary() | undefined,
    task_id :: binary(),                    % Required
    id :: binary()                          % Required
}).

-record(a2a_get_extended_agent_card_request, {
    tenant :: binary() | undefined
}).

%%% ==================================================================
%%% A2A Response Records
%%% ==================================================================

-record(a2a_send_message_response, {
    %% Discriminated union - only one should be set
    task :: #a2a_task{} | undefined,
    message :: #a2a_message{} | undefined
}).

-record(a2a_stream_response, {
    %% Discriminated union - only one should be set
    task :: #a2a_task{} | undefined,
    message :: #a2a_message{} | undefined,
    status_update :: #a2a_task_status_update_event{} | undefined,
    artifact_update :: #a2a_task_artifact_update_event{} | undefined
}).

-record(a2a_list_tasks_response, {
    tasks :: [#a2a_task{}],                 % Required
    next_page_token :: binary(),            % Required (empty if no more)
    page_size :: integer(),                 % Required
    total_size :: integer()                 % Required
}).

-record(a2a_list_push_configs_response, {
    configs :: [#a2a_task_push_notification_config{}],
    next_page_token :: binary() | undefined
}).

%%% ==================================================================
%%% A2A Server State Record
%%% ==================================================================

-record(a2a_server_state, {
    %% Agent identity
    agent_card :: #a2a_agent_card{} | undefined,
    extended_agent_card :: #a2a_agent_card{} | undefined,

    %% Tasks indexed by ID
    tasks :: #{binary() => #a2a_task{}},

    %% Contexts indexed by ID (contain task lists)
    contexts :: #{binary() => [binary()]},  % context_id => [task_ids]

    %% Push notification configs indexed by task_id
    push_configs :: #{binary() => #{binary() => #a2a_task_push_notification_config{}}},

    %% Active subscriptions: task_id => [subscriber_pid]
    subscriptions :: #{binary() => [pid()]},

    %% Streaming sessions
    streams :: #{binary() => pid()},        % stream_id => stream_pid

    %% Skill handlers
    skill_handlers :: #{binary() => fun()}, % skill_id => handler_fun

    %% Configuration
    tenant :: binary() | undefined,
    capabilities :: #a2a_agent_capabilities{}
}).

%%% ==================================================================
%%% Export Types
%%% ==================================================================

-export_type([
    a2a_task_state/0,
    a2a_role/0,
    a2a_part_content_type/0,
    a2a_security_scheme_type/0,
    a2a_oauth_flow_type/0
]).

-endif.
