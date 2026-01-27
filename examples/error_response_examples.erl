%%%-------------------------------------------------------------------
%%% @doc
%%% Example usage of the MCP JSON-RPC error response structure with
%%% proper data field context for all error types.
%%%
%%% This module demonstrates best practices for error handling in
%%% both server and client implementations.
%%% @end
%%%-------------------------------------------------------------------

-module(error_response_examples).

-include("erlmcp.hrl").

%% Example functions
-export([
    example_parse_error/0,
    example_invalid_request/0,
    example_invalid_params/0,
    example_method_not_found/0,
    example_internal_error/0,
    example_version_mismatch/0,
    example_feature_not_negotiated/0,
    example_resource_not_found/0,
    example_tool_not_found/0,
    example_server_error_handling/0,
    example_error_creation/0
]).

%%====================================================================
%% Example 1: Parse Error
%%====================================================================

%% When the JSON parser encounters invalid syntax, include parsing context
example_parse_error() ->
    Json = <<"{ invalid json ">>,

    % Instead of just encoding a generic parse error...
    % erlmcp_json_rpc:encode_error_response(null, ?JSONRPC_PARSE_ERROR, ?JSONRPC_MSG_PARSE_ERROR),

    % Include diagnostic information
    Data = #{
        <<"parsing_error">> => <<"Unexpected character '}' at position 15">>,
        <<"line">> => 1,
        <<"column">> => 15
    },

    erlmcp_json_rpc:encode_error_response(
        null,  % null ID for unparseable requests
        ?JSONRPC_PARSE_ERROR,
        ?JSONRPC_MSG_PARSE_ERROR,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": null,
%   "error": {
%     "code": -32700,
%     "message": "Parse error",
%     "data": {
%       "parsing_error": "Unexpected character '}' at position 15",
%       "line": 1,
%       "column": 15
%     }
%   }
% }

%%====================================================================
%% Example 2: Invalid Request (Missing Field)
%%====================================================================

%% When a required field is missing, include field information
example_invalid_request() ->
    % Received request missing "method" field
    Data = #{
        <<"field">> => <<"method">>,
        <<"reason">> => <<"The 'method' field is required in all requests">>
    },

    erlmcp_json_rpc:encode_error_response(
        1,  % request had an ID
        ?JSONRPC_INVALID_REQUEST,
        ?JSONRPC_MSG_INVALID_REQUEST,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": 1,
%   "error": {
%     "code": -32600,
%     "message": "Invalid Request",
%     "data": {
%       "field": "method",
%       "reason": "The 'method' field is required in all requests"
%     }
%   }
% }

%%====================================================================
%% Example 3: Invalid Params (Type Error)
%%====================================================================

%% When parameters have wrong type, include field and type info
example_invalid_params() ->
    % Client sent {uri: 123} instead of {uri: "string"}
    Data = #{
        <<"field">> => <<"uri">>,
        <<"expected_type">> => <<"string">>,
        <<"received_type">> => <<"number">>,
        <<"received_value">> => 123
    },

    erlmcp_json_rpc:encode_error_response(
        <<"req-5">>,
        ?JSONRPC_INVALID_PARAMS,
        ?JSONRPC_MSG_INVALID_PARAMS,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": "req-5",
%   "error": {
%     "code": -32602,
%     "message": "Invalid params",
%     "data": {
%       "field": "uri",
%       "expected_type": "string",
%       "received_type": "number",
%       "received_value": 123
%     }
%   }
% }

%%====================================================================
%% Example 4: Method Not Found
%%====================================================================

%% When method doesn't exist, include method name
example_method_not_found() ->
    Data = #{
        <<"method">> => <<"invalid_method">>,
        <<"available_methods">> => [
            <<"initialize">>,
            <<"resources/list">>,
            <<"tools/call">>
        ]
    },

    erlmcp_json_rpc:encode_error_response(
        2,
        ?JSONRPC_METHOD_NOT_FOUND,
        ?JSONRPC_MSG_METHOD_NOT_FOUND,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": 2,
%   "error": {
%     "code": -32601,
%     "message": "Method not found",
%     "data": {
%       "method": "invalid_method",
%       "available_methods": [
%         "initialize",
%         "resources/list",
%         "tools/call"
%       ]
%     }
%   }
% }

%%====================================================================
%% Example 5: Internal Error
%%====================================================================

%% When unexpected error occurs, include diagnostic details
example_internal_error() ->
    Data = #{
        <<"details">> => <<"Database connection timeout after 5000ms">>,
        <<"service">> => <<"resource_database">>,
        <<"error_code">> => <<"TIMEOUT">>
    },

    erlmcp_json_rpc:encode_error_response(
        3,
        ?JSONRPC_INTERNAL_ERROR,
        ?JSONRPC_MSG_INTERNAL_ERROR,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": 3,
%   "error": {
%     "code": -32603,
%     "message": "Internal error",
%     "data": {
%       "details": "Database connection timeout after 5000ms",
%       "service": "resource_database",
%       "error_code": "TIMEOUT"
%     }
%   }
% }

%%====================================================================
%% Example 6: Version Mismatch
%%====================================================================

%% When client sends unsupported protocol version
example_version_mismatch() ->
    Data = #{
        <<"supported">> => [<<"2025-06-18">>, <<"2024-11-05">>],
        <<"requested">> => <<"2023-01-01">>,
        <<"minimum_required">> => <<"2024-11-05">>
    },

    erlmcp_json_rpc:encode_error_response(
        <<"init-1">>,
        ?JSONRPC_INVALID_REQUEST,
        <<"Unsupported protocol version">>,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": "init-1",
%   "error": {
%     "code": -32600,
%     "message": "Unsupported protocol version",
%     "data": {
%       "supported": ["2025-06-18", "2024-11-05"],
%       "requested": "2023-01-01",
%       "minimum_required": "2024-11-05"
%     }
%   }
% }

%%====================================================================
%% Example 7: Feature Not Negotiated
%%====================================================================

%% When client uses feature not negotiated in capabilities
example_feature_not_negotiated() ->
    Data = #{
        <<"feature">> => <<"resources">>,
        <<"reason">> => <<"Server does not support resources capability">>,
        <<"available_features">> => [<<"tools">>, <<"prompts">>]
    },

    erlmcp_json_rpc:encode_error_response(
        4,
        ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED,
        ?MCP_MSG_CAPABILITY_NOT_SUPPORTED,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": 4,
%   "error": {
%     "code": -32004,
%     "message": "Capability not supported",
%     "data": {
%       "feature": "resources",
%       "reason": "Server does not support resources capability",
%       "available_features": ["tools", "prompts"]
%     }
%   }
% }

%%====================================================================
%% Example 8: Resource Not Found
%%====================================================================

%% When requested resource doesn't exist
example_resource_not_found() ->
    Data = #{
        <<"uri">> => <<"file:///missing/resource.txt">>,
        <<"resource_type">> => <<"file">>,
        <<"reason">> => <<"File does not exist or is not readable">>
    },

    erlmcp_json_rpc:encode_error_response(
        5,
        ?MCP_ERROR_RESOURCE_NOT_FOUND,
        ?MCP_MSG_RESOURCE_NOT_FOUND,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": 5,
%   "error": {
%     "code": -32001,
%     "message": "Resource not found",
%     "data": {
%       "uri": "file:///missing/resource.txt",
%       "resource_type": "file",
%       "reason": "File does not exist or is not readable"
%     }
%   }
% }

%%====================================================================
%% Example 9: Tool Not Found
%%====================================================================

%% When requested tool doesn't exist
example_tool_not_found() ->
    Data = #{
        <<"tool_name">> => <<"summarize_text">>,
        <<"available_tools">> => [
            <<"search_web">>,
            <<"send_email">>,
            <<"create_file">>
        ],
        <<"did_you_mean">> => <<"summarize_text_brief">>
    },

    erlmcp_json_rpc:encode_error_response(
        6,
        ?MCP_ERROR_TOOL_NOT_FOUND,
        ?MCP_MSG_TOOL_NOT_FOUND,
        Data
    ).

% Output JSON:
% {
%   "jsonrpc": "2.0",
%   "id": 6,
%   "error": {
%     "code": -32002,
%     "message": "Tool not found",
%     "data": {
%       "tool_name": "summarize_text",
%       "available_tools": [
%         "search_web",
%         "send_email",
%         "create_file"
%       ],
%       "did_you_mean": "summarize_text_brief"
%     }
%   }
% }

%%====================================================================
%% Example 10: Server-Side Error Handling Pattern
%%====================================================================

%% Realistic server-side error handling in handle_request
example_server_error_handling() ->
    % Simulating server code that validates and handles errors
    handle_resource_read_request(
        1,                          % request ID
        <<"unknown.txt">>,          % URI
        undefined                   % transport ID
    ).

%% Private helper
handle_resource_read_request(ReqId, Uri, _TransportId) ->
    % Validate the URI format
    case validate_uri(Uri) of
        {error, invalid_format} ->
            Data = #{
                <<"field">> => <<"uri">>,
                <<"format">> => <<"scheme://path">>,
                <<"received">> => Uri
            },
            erlmcp_json_rpc:encode_error_response(
                ReqId,
                ?JSONRPC_INVALID_PARAMS,
                <<"Invalid URI format">>,
                Data
            );

        {error, missing_scheme} ->
            Data = #{
                <<"field">> => <<"uri">>,
                <<"reason">> => <<"URI must include a scheme (e.g., file://, http://)">>
            },
            erlmcp_json_rpc:encode_error_response(
                ReqId,
                ?JSONRPC_INVALID_PARAMS,
                <<"Missing URI scheme">>,
                Data
            );

        ok ->
            % Check if resource exists
            case find_resource(Uri) of
                {ok, Content} ->
                    erlmcp_json_rpc:encode_response(ReqId, #{
                        <<"uri">> => Uri,
                        <<"content">> => Content
                    });

                {error, not_found} ->
                    Data = #{
                        <<"uri">> => Uri,
                        <<"reason">> => <<"Resource does not exist">>
                    },
                    erlmcp_json_rpc:encode_error_response(
                        ReqId,
                        ?MCP_ERROR_RESOURCE_NOT_FOUND,
                        ?MCP_MSG_RESOURCE_NOT_FOUND,
                        Data
                    );

                {error, permission_denied} ->
                    Data = #{
                        <<"uri">> => Uri,
                        <<"reason">> => <<"Permission denied">>,
                        <<"required_permission">> => <<"read">>
                    },
                    erlmcp_json_rpc:encode_error_response(
                        ReqId,
                        ?JSONRPC_INTERNAL_ERROR,
                        <<"Cannot access resource">>,
                        Data
                    )
            end
    end.

%% Helper functions
validate_uri(Uri) when is_binary(Uri) ->
    case binary:match(Uri, <<"://">>) of
        nomatch -> {error, missing_scheme};
        {_Pos, _Len} -> ok
    end;
validate_uri(_) ->
    {error, invalid_format}.

find_resource(_Uri) ->
    % Simulate resource lookup
    {error, not_found}.

%%====================================================================
%% Example 11: Using Error Creation Helpers
%%====================================================================

%% Convenient error creation with helper functions
example_error_creation() ->
    % Using basic error creation
    Error1 = erlmcp_json_rpc:create_error(
        ?JSONRPC_INVALID_PARAMS,
        ?JSONRPC_MSG_INVALID_PARAMS,
        #{<<"field">> => <<"uri">>}
    ),

    % Using atom-key helper
    Error2 = erlmcp_json_rpc:create_error_with_data(
        ?JSONRPC_INVALID_PARAMS,
        ?JSONRPC_MSG_INVALID_PARAMS,
        field,
        <<"uri">>
    ),

    % Both create the same structure
    {Error1, Error2}.

%%====================================================================
%% Best Practices Summary
%%====================================================================

% 1. ALWAYS include relevant context in the data field
%    - Field names for validation errors
%    - Error positions for parse errors
%    - Actual values for type errors
%    - Available alternatives for not found errors

% 2. USE CONSISTENT ERROR CODES
%    - Standard JSON-RPC codes for protocol errors
%    - MCP server error codes for domain-specific errors
%    - Match code with appropriate data format

% 3. PROVIDE ACTIONABLE MESSAGES
%    - Explain what went wrong
%    - Suggest how to fix it
%    - Avoid generic "error occurred" messages

% 4. STRUCTURE DATA CONSISTENTLY
%    - Use maps for all complex data
%    - Use snake_case for map keys
%    - Keep keys short but descriptive

% 5. CONSIDER CLIENT EXPERIENCE
%    - Include suggestions (did_you_mean)
%    - List available options
%    - Explain protocol constraints

% 6. HANDLE EDGE CASES
%    - Always provide ID in response if available
%    - Use null for unparseable requests
%    - Include recovery suggestions

% 7. LOG FOR DEBUGGING
%    - Log full error context server-side
%    - Return sanitized messages to clients
%    - Track error patterns for improvements
