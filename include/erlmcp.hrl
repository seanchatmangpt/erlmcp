-ifndef(ERLMCP_HRL).
-define(ERLMCP_HRL, true).

-define(MCP_VERSION, <<"2025-06-18">>).

-type json_rpc_id() :: binary() | integer() | null.
-type json_rpc_method() :: binary().
-type json_rpc_params() :: map() | list() | undefined.

-record(json_rpc_request, {
    jsonrpc = <<"2.0">> :: binary(),
    id :: json_rpc_id(),
    method :: json_rpc_method(),
    params :: json_rpc_params()
}).

-record(json_rpc_response, {
    jsonrpc = <<"2.0">> :: binary(),
    id :: json_rpc_id(),
    result :: term(),
    error :: map() | undefined
}).

-record(json_rpc_notification, {
    jsonrpc = <<"2.0">> :: binary(),
    method :: json_rpc_method(),
    params :: json_rpc_params()
}).

-record(mcp_error, {
    code :: integer(),
    message :: binary(),
    data :: term()
}).

-record(mcp_capability, {
    name :: binary(),
    enabled :: boolean()
}).

-record(mcp_client_capabilities, {
    roots :: #mcp_capability{} | undefined,
    sampling :: #mcp_capability{} | undefined,
    experimental :: map() | undefined
}).

-record(mcp_server_capabilities, {
    prompts :: #mcp_capability{} | undefined,
    resources :: #mcp_capability{} | undefined,
    tools :: #mcp_capability{} | undefined,
    logging :: #mcp_capability{} | undefined
}).

-record(mcp_initialize_request, {
    protocol_version :: binary(),
    capabilities :: #mcp_client_capabilities{} | #mcp_server_capabilities{},
    client_info :: map() | undefined
}).

-record(mcp_initialize_response, {
    protocol_version :: binary(),
    capabilities :: #mcp_server_capabilities{} | #mcp_client_capabilities{},
    server_info :: map() | undefined
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

-record(mcp_tool_annotation, {
    audience :: [binary()],
    priority :: float() | undefined
}).

-record(mcp_prompt_argument, {
    name :: binary(),
    description :: binary() | undefined,
    required :: boolean()
}).

-record(mcp_prompt, {
    name :: binary(),
    description :: binary() | undefined,
    arguments :: [#mcp_prompt_argument{}] | undefined
}).

-record(mcp_content, {
    type :: binary(),
    text :: binary() | undefined,
    data :: binary() | undefined,
    mime_type :: binary() | undefined
}).

-record(mcp_progress_token, {
    token :: binary() | integer()
}).

-record(mcp_progress_notification, {
    progress_token :: #mcp_progress_token{},
    progress :: float(),
    total :: float() | undefined
}).

-record(mcp_resource_subscription, {
    uri :: binary(),
    subscriber :: pid()
}).

-record(mcp_log_message, {
    level :: binary(),
    data :: term(),
    logger :: binary() | undefined
}).

-endif.
