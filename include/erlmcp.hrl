%%% MCP Protocol Version
-define(MCP_VERSION, <<"2025-06-18">>).

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
-record(mcp_capability, {
    enabled = false :: boolean()
}).

-record(mcp_client_capabilities, {
    roots :: #mcp_capability{} | undefined,
    sampling :: #mcp_capability{} | undefined,
    experimental :: map() | undefined
}).

-record(mcp_server_capabilities, {
    resources :: #mcp_capability{} | undefined,
    tools :: #mcp_capability{} | undefined,
    prompts :: #mcp_capability{} | undefined,
    logging :: #mcp_capability{} | undefined
}).

%%% MCP Content Records
-record(mcp_content, {
    type :: binary(),
    text :: binary() | undefined,
    data :: binary() | undefined,
    mime_type :: binary() | undefined
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
    arguments :: [#mcp_prompt_argument{}] | undefined
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