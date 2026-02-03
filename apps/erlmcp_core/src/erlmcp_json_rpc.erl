-module(erlmcp_json_rpc).

-include("erlmcp.hrl").

%% Import unwrap utilities for safe nested access
-import(erlmcp_unwrap_utils, [
    extract_rpc_response/1,
    extract_rpc_error/1,
    extract_nested/3,
    get_in/2
]).

%% API exports
-export([encode_request/3, encode_response/2, encode_error_response/3, encode_error_response/4,
         encode_notification/2, decode_message/1, decode_message/2, decode_batch/1, encode_batch/1,
         is_batch_request/1, create_error/3, validate_error_code/1,
         error_method_not_found/2, error_invalid_params/2, error_resource_not_found/2,
         error_tool_not_found/2, error_prompt_not_found/2, error_capability_not_supported/2,
         error_not_initialized/1, error_validation_failed/2, error_message_too_large/2,
         error_internal/1, error_parse/1, create_batch_error_response/3, error_severity/1, is_mcp_core_error/1, is_mcp_content_error/1,
         is_mcp_resource_error/1, is_mcp_tool_error/1, is_mcp_prompt_error/1, is_mcp_auth_error/1,
         is_mcp_protocol_error/1, is_mcp_pagination_error/1, is_mcp_task_error/1,
         is_mcp_progress_error/1, is_mcp_completion_error/1, error_elicitation_failed/2,
         error_elicitation_timeout/2, error_elicitation_cancelled/1,
         error_invalid_elicitation_mode/2, error_elicitation_security_error/2,
         error_experimental_task_not_found/2,
         error_experimental_task_cancelled/2,
         %% Safe extraction helpers
         safe_extract_result/1, safe_extract_error/1, safe_extract_nested/3, safe_get_field/2,
         %% UTF-8 support
         validate_utf8/1, ensure_utf8_encoding/1]).


%% Types
-type json_rpc_message() :: #json_rpc_request{} | #json_rpc_response{} | #json_rpc_notification{}.
-type decode_result() :: {ok, json_rpc_message()} | {error, {atom(), term()}}.
-type batch_request() :: [json_rpc_message()].
-type batch_decode_result() :: {ok, batch_request()} | {error, {atom(), term()}}.

-export_type([json_rpc_message/0, batch_request/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec encode_request(json_rpc_id(), binary(), json_rpc_params()) -> binary().
encode_request(Id, Method, Params) when is_binary(Method) ->
    Request =
        #json_rpc_request{id = Id,
                          method = Method,
                          params = Params},
    encode_message(Request).

-spec encode_response(json_rpc_id(), term()) -> binary().
encode_response(Id, Result) ->
    Response = #json_rpc_response{id = Id, result = Result},
    encode_message(Response).

-spec encode_error_response(json_rpc_id(), integer(), binary()) -> binary().
encode_error_response(Id, Code, Message) when is_integer(Code), is_binary(Message) ->
    encode_error_response(Id, Code, Message, undefined).

-spec encode_error_response(json_rpc_id(), integer(), binary(), term()) -> binary().
encode_error_response(Id, Code, Message, Data) when is_integer(Code), is_binary(Message) ->
    %% Validate error code - use internal error if invalid
    FinalCode =
        case validate_error_code(Code) of
            true ->
                Code;
            false ->
                logger:warning("Invalid error code ~p, using internal error", [Code]),
                ?JSONRPC_INTERNAL_ERROR
        end,
    Error = build_error_object(FinalCode, Message, Data),
    Response = #json_rpc_response{id = Id, error = Error},
    encode_message(Response).

-spec encode_notification(binary(), json_rpc_params()) -> binary().
encode_notification(Method, Params) when is_binary(Method) ->
    Notification = #json_rpc_notification{method = Method, params = Params},
    encode_message(Notification).

-spec decode_message(binary()) -> decode_result().
decode_message(Json) when is_binary(Json) ->
    decode_message(Json, default).

%% @doc Decode JSON-RPC message with optional size validation
%% Validates message size before decoding if a transport type is specified
-spec decode_message(binary(), atom() | default) -> decode_result().
decode_message(Json, TransportType) when is_binary(Json) ->
    %% Validate message size first (Gap #45: Message Size Limits)
    case erlmcp_message_size:validate_message_size(TransportType, Json) of
        ok ->
            try erlmcp_json_native:decode(Json) of
                Data when is_map(Data) ->
                    erlmcp_message_parser:parse_json_rpc(Data);
                Data when is_list(Data) ->
                    %% Batch request detected - should use decode_batch instead
                    {error, {batch_request, length(Data)}};
                _ ->
                    {error, {invalid_json, not_object}}
            catch
                error:badarg ->
                    {error, {parse_error, invalid_json}};
                Class:Reason ->
                    {error, {parse_error, {Class, Reason}}}
            end;
        {error, {message_too_large, ErrorResponse}} ->
            {error, {message_too_large, ErrorResponse}}
    end.

-spec decode_batch(binary()) -> batch_decode_result().
decode_batch(Json) when is_binary(Json) ->
    try erlmcp_json_native:decode(Json) of
        Data when is_list(Data) ->
            parse_batch(Data);
        Data when is_map(Data) ->
            %% Single request, wrap in list
            case erlmcp_message_parser:parse_json_rpc(Data) of
                {ok, Message} ->
                    {ok, [Message]};
                Error ->
                    Error
            end;
        _ ->
            {error, {invalid_json, not_array_or_object}}
    catch
        error:badarg ->
            {error, {parse_error, invalid_json}};
        Class:Reason ->
            {error, {parse_error, {Class, Reason}}}
    end.

-spec encode_batch([json_rpc_message()]) -> binary().
encode_batch(Messages) when is_list(Messages) ->
    Maps = [build_message_map(Msg) || Msg <- Messages],
    erlmcp_json_native:encode(Maps).

-spec is_batch_request(binary()) -> boolean().
is_batch_request(Json) when is_binary(Json) ->
    try
        case erlmcp_json_native:decode(Json) of
            L when is_list(L) ->
                true;
            _ ->
                false
        end
    catch
        _:_ ->
            false
    end.

-spec create_error(integer(), binary(), term()) -> #mcp_error{}.
create_error(Code, Message, Data) when is_integer(Code), is_binary(Message) ->
    #mcp_error{code = Code,
               message = Message,
               data = Data}.

-spec create_error_with_data(integer(), binary(), atom(), term()) -> #mcp_error{}.
create_error_with_data(Code, Message, DataKey, DataValue)
    when is_integer(Code), is_binary(Message), is_atom(DataKey) ->
    #mcp_error{code = Code,
               message = Message,
               data = #{atom_to_binary(DataKey, utf8) => DataValue}}.

%%====================================================================
%% Error Code Validation
%%====================================================================

-spec validate_error_code(integer()) -> boolean().
validate_error_code(Code) when is_integer(Code) ->
    lists:member(Code, ?VALID_ERROR_CODES).

%%====================================================================
%% Error Code Classification Functions
%%====================================================================

%% @doc Get severity level for an error code
%% Returns: critical | error | warning | info
-spec error_severity(integer()) -> critical | error | warning | info.
error_severity(Code) when Code =:= -32700; Code =:= -32600; Code =:= -32603 ->
    critical;  % Parse errors, invalid request, internal errors are critical
error_severity(Code) when Code =:= -32601; Code =:= -32602 ->
    error;     % Method not found, invalid params are standard errors
error_severity(Code) when Code >= -32099, Code =< -32080 ->
    error;     % MCP core through pagination errors are errors
error_severity(Code) when Code >= -32079, Code =< -32000 ->
    warning;   % Task, progress, completion errors are warnings (recoverable)
error_severity(Code) when Code >= -32113, Code =< -32110 ->
    warning;   % Completion errors are warnings (recoverable)
error_severity(_Code) ->
    info.      % Unknown codes are info level

%% @doc Get error category for an error code
%% Returns: jsonrpc | mcp_core | content | resource | tool | prompt |
%%          auth | protocol | pagination | task | progress | completion | unknown
-spec error_category(integer()) ->
                        jsonrpc |
                        mcp_core |
                        content |
                        resource |
                        tool |
                        prompt |
                        auth |
                        protocol |
                        pagination |
                        task |
                        progress |
                        completion |
                        unknown.
error_category(Code) when Code >= -32700, Code =< -32600 ->
    jsonrpc;   % JSON-RPC 2.0 standard errors
error_category(Code) when Code >= -32010, Code =< -32001 ->
    mcp_core;  % Core MCP errors
error_category(Code) when Code >= -32020, Code =< -32011 ->
    content;   % Content and message errors
error_category(Code) when Code >= -32030, Code =< -32021 ->
    resource;  % Resource and template errors
error_category(Code) when Code >= -32040, Code =< -32031 ->
    tool;      % Tool and execution errors
error_category(Code) when Code >= -32050, Code =< -32041 ->
    prompt;    % Prompt and sampling errors
error_category(Code) when Code >= -32060, Code =< -32051 ->
    auth;      % Authentication and authorization errors
error_category(Code) when Code >= -32070, Code =< -32061 ->
    protocol;  % Protocol and negotiation errors
error_category(Code) when Code >= -32080, Code =< -32071 ->
    pagination; % Pagination and cursor errors
error_category(Code) when Code >= -32090, Code =< -32081 ->
    task;      % Task and job errors
error_category(Code) when Code >= -32100, Code =< -32091 ->
    progress;  % Progress and notification errors
error_category(Code) when Code >= -32113, Code =< -32110 ->
    completion; % Completion errors
error_category(Code) when Code =:= -32000 ->
    mcp_core;  % Custom server error
error_category(_Code) ->
    unknown.   % Unknown error code

%% @doc Check if error code is a JSON-RPC 2.0 standard error
-spec is_jsonrpc_standard_error(integer()) -> boolean().
is_jsonrpc_standard_error(Code) when Code >= -32700, Code =< -32600 ->
    true;
is_jsonrpc_standard_error(_Code) ->
    false.

%% @doc Check if error code is a core MCP error
-spec is_mcp_core_error(integer()) -> boolean().
is_mcp_core_error(Code) when Code >= -32010, Code =< -32001 ->
    true;
is_mcp_core_error(-32000) ->
    true;  % Custom server error
is_mcp_core_error(_Code) ->
    false.

%% @doc Check if error code is a content error
-spec is_mcp_content_error(integer()) -> boolean().
is_mcp_content_error(Code) when Code >= -32020, Code =< -32011 ->
    true;
is_mcp_content_error(_Code) ->
    false.

%% @doc Check if error code is a resource error
-spec is_mcp_resource_error(integer()) -> boolean().
is_mcp_resource_error(Code) when Code >= -32030, Code =< -32021 ->
    true;
is_mcp_resource_error(_Code) ->
    false.

%% @doc Check if error code is a tool error
-spec is_mcp_tool_error(integer()) -> boolean().
is_mcp_tool_error(Code) when Code >= -32040, Code =< -32031 ->
    true;
is_mcp_tool_error(_Code) ->
    false.

%% @doc Check if error code is a prompt error
-spec is_mcp_prompt_error(integer()) -> boolean().
is_mcp_prompt_error(Code) when Code >= -32050, Code =< -32041 ->
    true;
is_mcp_prompt_error(_Code) ->
    false.

%% @doc Check if error code is an auth error
-spec is_mcp_auth_error(integer()) -> boolean().
is_mcp_auth_error(Code) when Code >= -32060, Code =< -32051 ->
    true;
is_mcp_auth_error(_Code) ->
    false.

%% @doc Check if error code is a protocol error
-spec is_mcp_protocol_error(integer()) -> boolean().
is_mcp_protocol_error(Code) when Code >= -32070, Code =< -32061 ->
    true;
is_mcp_protocol_error(_Code) ->
    false.

%% @doc Check if error code is a pagination error
-spec is_mcp_pagination_error(integer()) -> boolean().
is_mcp_pagination_error(Code) when Code >= -32080, Code =< -32071 ->
    true;
is_mcp_pagination_error(_Code) ->
    false.

%% @doc Check if error code is a task error
-spec is_mcp_task_error(integer()) -> boolean().
is_mcp_task_error(Code) when Code >= -32090, Code =< -32081 ->
    true;
is_mcp_task_error(_Code) ->
    false.

%% @doc Check if error code is a progress error
-spec is_mcp_progress_error(integer()) -> boolean().
is_mcp_progress_error(Code) when Code >= -32100, Code =< -32091 ->
    true;
is_mcp_progress_error(_Code) ->
    false.

%% @doc Check if error code is a completion error
-spec is_mcp_completion_error(integer()) -> boolean().
is_mcp_completion_error(Code) when Code >= -32113, Code =< -32110 ->
    true;
is_mcp_completion_error(_Code) ->
    false.

%%====================================================================
%% Experimental Error Helper Functions (1090-1099)
%%====================================================================

%% Elicitation failed error
-spec error_elicitation_failed(json_rpc_id(), binary()) -> binary().
error_elicitation_failed(Id, Reason) when is_binary(Reason) ->
    Data = #{<<"reason">> => Reason},
    encode_error_response(Id, ?ELICITATION_FAILED, ?MSG_ELICITATION_FAILED, Data).

%% Elicitation timeout error
-spec error_elicitation_timeout(json_rpc_id(), pos_integer()) -> binary().
error_elicitation_timeout(Id, TimeoutMs) when is_integer(TimeoutMs) ->
    Data = #{<<"timeoutMs">> => TimeoutMs},
    encode_error_response(Id, ?ELICITATION_TIMEOUT, ?MSG_ELICITATION_TIMEOUT, Data).

%% Elicitation cancelled error
-spec error_elicitation_cancelled(json_rpc_id()) -> binary().
error_elicitation_cancelled(Id) ->
    encode_error_response(Id, ?ELICITATION_CANCELLED, ?MSG_ELICITATION_CANCELLED, undefined).

%% Invalid elicitation mode error
-spec error_invalid_elicitation_mode(json_rpc_id(), binary()) -> binary().
error_invalid_elicitation_mode(Id, Mode) when is_binary(Mode) ->
    Data = #{<<"mode">> => Mode},
    encode_error_response(Id, ?INVALID_ELICITATION_MODE, ?MSG_INVALID_ELICITATION_MODE, Data).

%% Elicitation security error
-spec error_elicitation_security_error(json_rpc_id(), binary()) -> binary().
error_elicitation_security_error(Id, Reason) when is_binary(Reason) ->
    Data = #{<<"reason">> => Reason},
    encode_error_response(Id, ?ELICITATION_SECURITY_ERROR, ?MSG_ELICITATION_SECURITY_ERROR, Data).

%% Experimental task not found error (positive code 1095)
-spec error_experimental_task_not_found(json_rpc_id(), binary()) -> binary().
error_experimental_task_not_found(Id, TaskId) when is_binary(TaskId) ->
    Data = #{<<"taskId">> => TaskId},
    encode_error_response(Id, ?TASK_NOT_FOUND, ?MSG_TASK_NOT_FOUND, Data).

%% Task dependency failed error
-spec error_task_dependency_failed(json_rpc_id(), binary(), binary()) -> binary().
error_task_dependency_failed(Id, TaskId, DependencyId)
    when is_binary(TaskId), is_binary(DependencyId) ->
    Data = #{<<"taskId">> => TaskId, <<"dependencyId">> => DependencyId},
    encode_error_response(Id, ?TASK_DEPENDENCY_FAILED, ?MSG_TASK_DEPENDENCY_FAILED, Data).

%% Experimental task cancelled error (positive code 1097)
-spec error_experimental_task_cancelled(json_rpc_id(), binary()) -> binary().
error_experimental_task_cancelled(Id, TaskId) when is_binary(TaskId) ->
    Data = #{<<"taskId">> => TaskId},
    encode_error_response(Id, ?TASK_CANCELLED, ?MSG_TASK_CANCELLED, Data).

%% Experimental task timeout error (positive code 1098)
-spec error_experimental_task_timeout(json_rpc_id(), binary(), pos_integer()) -> binary().
error_experimental_task_timeout(Id, TaskId, TimeoutMs)
    when is_binary(TaskId), is_integer(TimeoutMs) ->
    Data = #{<<"taskId">> => TaskId, <<"timeoutMs">> => TimeoutMs},
    encode_error_response(Id, ?TASK_TIMEOUT, ?MSG_TASK_TIMEOUT, Data).

%% Invalid task state error
-spec error_invalid_task_state(json_rpc_id(), binary(), binary(), binary()) -> binary().
error_invalid_task_state(Id, TaskId, CurrentState, ExpectedState)
    when is_binary(TaskId), is_binary(CurrentState), is_binary(ExpectedState) ->
    Data =
        #{<<"taskId">> => TaskId,
          <<"currentState">> => CurrentState,
          <<"expectedState">> => ExpectedState},
    encode_error_response(Id, ?INVALID_TASK_STATE, ?MSG_INVALID_TASK_STATE, Data).

%%====================================================================
%% Error Helper Functions - Common MCP Error Types
%%====================================================================

%% Method not found error
-spec error_method_not_found(json_rpc_id(), binary()) -> binary().
error_method_not_found(Id, Method) when is_binary(Method) ->
    Data = #{<<"method">> => Method},
    encode_error_response(Id, ?JSONRPC_METHOD_NOT_FOUND, ?JSONRPC_MSG_METHOD_NOT_FOUND, Data).

%% Invalid parameters error
-spec error_invalid_params(json_rpc_id(), binary() | string() | list()) -> binary().
error_invalid_params(Id, Details) when is_list(Details) ->
    error_invalid_params(Id, erlang:list_to_binary(Details));
error_invalid_params(Id, Details) when is_atom(Details) ->
    error_invalid_params(Id, atom_to_binary(Details, utf8));
error_invalid_params(Id, Details) when is_binary(Details) ->
    Data = #{<<"details">> => Details},
    encode_error_response(Id, ?JSONRPC_INVALID_PARAMS, ?JSONRPC_MSG_INVALID_PARAMS, Data).

%% Resource not found error
-spec error_resource_not_found(json_rpc_id(), erlmcp_mcp_types:mcp_resource_uri()) -> binary().
error_resource_not_found(Id, Uri) when is_binary(Uri) ->
    Data = #{<<"uri">> => Uri},
    encode_error_response(Id, ?MCP_ERROR_RESOURCE_NOT_FOUND, ?MCP_MSG_RESOURCE_NOT_FOUND, Data).

%% Tool not found error
-spec error_tool_not_found(json_rpc_id(), erlmcp_mcp_types:mcp_tool_name()) -> binary().
error_tool_not_found(Id, ToolName) when is_binary(ToolName) ->
    Data = #{<<"tool">> => ToolName},
    encode_error_response(Id, ?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND, Data).

%% Prompt not found error
-spec error_prompt_not_found(json_rpc_id(), erlmcp_mcp_types:mcp_prompt_name()) -> binary().
error_prompt_not_found(Id, PromptName) when is_binary(PromptName) ->
    Data = #{<<"prompt">> => PromptName},
    encode_error_response(Id, ?MCP_ERROR_PROMPT_NOT_FOUND, ?MCP_MSG_PROMPT_NOT_FOUND, Data).

%% Capability not supported error
-spec error_capability_not_supported(json_rpc_id(), binary()) -> binary().
error_capability_not_supported(Id, Capability) when is_binary(Capability) ->
    Data = #{<<"capability">> => Capability},
    encode_error_response(Id,
                          ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED,
                          ?MCP_MSG_CAPABILITY_NOT_SUPPORTED,
                          Data).

%% Not initialized error
-spec error_not_initialized(json_rpc_id()) -> binary().
error_not_initialized(Id) ->
    encode_error_response(Id, ?MCP_ERROR_NOT_INITIALIZED, ?MCP_MSG_NOT_INITIALIZED, undefined).

%% Validation failed error
-spec error_validation_failed(json_rpc_id(), binary() | string() | list()) -> binary().
error_validation_failed(Id, Details) when is_list(Details) ->
    error_validation_failed(Id, erlang:list_to_binary(Details));
error_validation_failed(Id, Details) when is_atom(Details) ->
    error_validation_failed(Id, atom_to_binary(Details, utf8));
error_validation_failed(Id, Details) when is_binary(Details) ->
    Data = #{<<"details">> => Details},
    encode_error_response(Id, ?MCP_ERROR_VALIDATION_FAILED, <<"Validation failed">>, Data).

%% Internal error
-spec error_internal(json_rpc_id()) -> binary().
error_internal(Id) ->
    encode_error_response(Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR, undefined).

%% Parse error
-spec error_parse(json_rpc_id()) -> binary().
error_parse(Id) ->
    encode_error_response(Id, ?JSONRPC_PARSE_ERROR, ?JSONRPC_MSG_PARSE_ERROR, undefined).

%% Message too large error (Gap #45: Message Size Limits)
-spec error_message_too_large(json_rpc_id(), non_neg_integer()) -> binary().
error_message_too_large(Id, MaxSize) when is_integer(MaxSize), MaxSize > 0 ->
    Data = #{<<"maxSize">> => MaxSize, <<"unit">> => <<"bytes">>},
    encode_error_response(Id, ?MCP_ERROR_MESSAGE_TOO_LARGE, ?MCP_MSG_MESSAGE_TOO_LARGE, Data).

%%====================================================================
%% Content Error Helper Functions (-32011 to -32020)
%%====================================================================

%% Tool description too long error
-spec error_tool_description_too_large(json_rpc_id(), pos_integer(), pos_integer()) -> binary().
error_tool_description_too_large(Id, ActualSize, MaxSize)
    when is_integer(ActualSize), is_integer(MaxSize) ->
    Data = #{<<"actualSize">> => ActualSize, <<"maxSize">> => MaxSize},
    encode_error_response(Id,
                          ?MCP_ERROR_TOOL_DESCRIPTION_TOO_LONG,
                          ?MCP_MSG_TOOL_DESCRIPTION_TOO_LONG,
                          Data).

%% Invalid content type error
-spec error_invalid_content_type(json_rpc_id(), binary()) -> binary().
error_invalid_content_type(Id, ContentType) when is_binary(ContentType) ->
    Data = #{<<"contentType">> => ContentType},
    encode_error_response(Id, ?MCP_ERROR_INVALID_CONTENT_TYPE, ?MCP_MSG_INVALID_CONTENT_TYPE, Data).

%% Content too large error
-spec error_content_too_large(json_rpc_id(), pos_integer(), pos_integer()) -> binary().
error_content_too_large(Id, ActualSize, MaxSize) when is_integer(ActualSize), is_integer(MaxSize) ->
    Data = #{<<"actualSize">> => ActualSize, <<"maxSize">> => MaxSize},
    encode_error_response(Id, ?MCP_ERROR_CONTENT_TOO_LARGE, ?MCP_MSG_CONTENT_TOO_LARGE, Data).

%% Invalid encoding error
-spec error_invalid_encoding(json_rpc_id(), binary()) -> binary().
error_invalid_encoding(Id, Encoding) when is_binary(Encoding) ->
    Data = #{<<"encoding">> => Encoding},
    encode_error_response(Id, ?MCP_ERROR_INVALID_ENCODING, ?MCP_MSG_INVALID_ENCODING, Data).

%%====================================================================
%% Resource Error Helper Functions (-32021 to -32030)
%%====================================================================

%% Resource template not found error
-spec error_resource_template_not_found(json_rpc_id(), binary()) -> binary().
error_resource_template_not_found(Id, TemplateUri) when is_binary(TemplateUri) ->
    Data = #{<<"templateUri">> => TemplateUri},
    encode_error_response(Id,
                          ?MCP_ERROR_RESOURCE_TEMPLATE_NOT_FOUND,
                          ?MCP_MSG_RESOURCE_TEMPLATE_NOT_FOUND,
                          Data).

%% Invalid URI error
-spec error_invalid_uri(json_rpc_id(), binary()) -> binary().
error_invalid_uri(Id, Uri) when is_binary(Uri) ->
    Data = #{<<"uri">> => Uri},
    encode_error_response(Id, ?MCP_ERROR_INVALID_URI, ?MCP_MSG_INVALID_URI, Data).

%% URI syntax error
-spec error_uri_syntax_error(json_rpc_id(), binary(), binary()) -> binary().
error_uri_syntax_error(Id, Uri, Reason) when is_binary(Uri), is_binary(Reason) ->
    Data = #{<<"uri">> => Uri, <<"reason">> => Reason},
    encode_error_response(Id, ?MCP_ERROR_URI_SYNTAX_ERROR, ?MCP_MSG_URI_SYNTAX_ERROR, Data).

%% Resource access denied error
-spec error_resource_access_denied(json_rpc_id(), binary()) -> binary().
error_resource_access_denied(Id, Uri) when is_binary(Uri) ->
    Data = #{<<"uri">> => Uri},
    encode_error_response(Id,
                          ?MCP_ERROR_RESOURCE_ACCESS_DENIED,
                          ?MCP_MSG_RESOURCE_ACCESS_DENIED,
                          Data).

%% Template render failed error
-spec error_template_render_failed(json_rpc_id(), binary(), binary()) -> binary().
error_template_render_failed(Id, TemplateUri, Reason)
    when is_binary(TemplateUri), is_binary(Reason) ->
    Data = #{<<"templateUri">> => TemplateUri, <<"reason">> => Reason},
    encode_error_response(Id,
                          ?MCP_ERROR_TEMPLATE_RENDER_FAILED,
                          ?MCP_MSG_TEMPLATE_RENDER_FAILED,
                          Data).

%%====================================================================
%% Tool Error Helper Functions (-32031 to -32040)
%%====================================================================

%% Tool execution failed error
-spec error_tool_execution_failed(json_rpc_id(), binary(), binary()) -> binary().
error_tool_execution_failed(Id, ToolName, Reason) when is_binary(ToolName), is_binary(Reason) ->
    Data = #{<<"tool">> => ToolName, <<"reason">> => Reason},
    encode_error_response(Id,
                          ?MCP_ERROR_TOOL_EXECUTION_FAILED,
                          ?MCP_MSG_TOOL_EXECUTION_FAILED,
                          Data).

%% Tool timeout error
-spec error_tool_timeout(json_rpc_id(), binary(), pos_integer()) -> binary().
error_tool_timeout(Id, ToolName, TimeoutMs) when is_binary(ToolName), is_integer(TimeoutMs) ->
    Data = #{<<"tool">> => ToolName, <<"timeoutMs">> => TimeoutMs},
    encode_error_response(Id, ?MCP_ERROR_TOOL_TIMEOUT, ?MCP_MSG_TOOL_TIMEOUT, Data).

%% Tool cancelled error
-spec error_tool_cancelled(json_rpc_id(), binary()) -> binary().
error_tool_cancelled(Id, ToolName) when is_binary(ToolName) ->
    Data = #{<<"tool">> => ToolName},
    encode_error_response(Id, ?MCP_ERROR_TOOL_CANCELLED, ?MCP_MSG_TOOL_CANCELLED, Data).

%% Invalid tool arguments error
-spec error_invalid_tool_arguments(json_rpc_id(), binary(), binary()) -> binary().
error_invalid_tool_arguments(Id, ToolName, Details) when is_binary(ToolName), is_binary(Details) ->
    Data = #{<<"tool">> => ToolName, <<"details">> => Details},
    encode_error_response(Id,
                          ?MCP_ERROR_INVALID_TOOL_ARGUMENTS,
                          ?MCP_MSG_INVALID_TOOL_ARGUMENTS,
                          Data).

%%====================================================================
%% Prompt Error Helper Functions (-32041 to -32050)
%%====================================================================

%% Prompt argument missing error
-spec error_prompt_argument_missing(json_rpc_id(), binary(), binary()) -> binary().
error_prompt_argument_missing(Id, PromptName, ArgName)
    when is_binary(PromptName), is_binary(ArgName) ->
    Data = #{<<"prompt">> => PromptName, <<"argument">> => ArgName},
    encode_error_response(Id,
                          ?MCP_ERROR_PROMPT_ARGUMENT_MISSING,
                          ?MCP_MSG_PROMPT_ARGUMENT_MISSING,
                          Data).

%% Prompt render failed error
-spec error_prompt_render_failed(json_rpc_id(), binary(), binary()) -> binary().
error_prompt_render_failed(Id, PromptName, Reason) when is_binary(PromptName), is_binary(Reason) ->
    Data = #{<<"prompt">> => PromptName, <<"reason">> => Reason},
    encode_error_response(Id, ?MCP_ERROR_PROMPT_RENDER_FAILED, ?MCP_MSG_PROMPT_RENDER_FAILED, Data).

%% Invalid prompt arguments error
-spec error_invalid_prompt_arguments(json_rpc_id(), binary(), binary()) -> binary().
error_invalid_prompt_arguments(Id, PromptName, Details)
    when is_binary(PromptName), is_binary(Details) ->
    Data = #{<<"prompt">> => PromptName, <<"details">> => Details},
    encode_error_response(Id,
                          ?MCP_ERROR_INVALID_PROMPT_ARGUMENTS,
                          ?MCP_MSG_INVALID_PROMPT_ARGUMENTS,
                          Data).

%% Sampling failed error
-spec error_sampling_failed(json_rpc_id(), binary()) -> binary().
error_sampling_failed(Id, Reason) when is_binary(Reason) ->
    Data = #{<<"reason">> => Reason},
    encode_error_response(Id, ?MCP_ERROR_SAMPLING_FAILED, ?MCP_MSG_SAMPLING_FAILED, Data).

%%====================================================================
%% Authentication Error Helper Functions (-32051 to -32060)
%%====================================================================

%% Authentication failed error
-spec error_authentication_failed(json_rpc_id(), binary()) -> binary().
error_authentication_failed(Id, Reason) when is_binary(Reason) ->
    Data = #{<<"reason">> => Reason},
    encode_error_response(Id,
                          ?MCP_ERROR_AUTHENTICATION_FAILED,
                          ?MCP_MSG_AUTHENTICATION_FAILED,
                          Data).

%% Authorization failed error
-spec error_authorization_failed(json_rpc_id(), binary()) -> binary().
error_authorization_failed(Id, Reason) when is_binary(Reason) ->
    Data = #{<<"reason">> => Reason},
    encode_error_response(Id, ?MCP_ERROR_AUTHORIZATION_FAILED, ?MCP_MSG_AUTHORIZATION_FAILED, Data).

%% Invalid credentials error
-spec error_invalid_credentials(json_rpc_id()) -> binary().
error_invalid_credentials(Id) ->
    encode_error_response(Id,
                          ?MCP_ERROR_INVALID_CREDENTIALS,
                          ?MCP_MSG_INVALID_CREDENTIALS,
                          undefined).

%% Token expired error
-spec error_token_expired(json_rpc_id()) -> binary().
error_token_expired(Id) ->
    encode_error_response(Id, ?MCP_ERROR_TOKEN_EXPIRED, ?MCP_MSG_TOKEN_EXPIRED, undefined).

%% Access denied error
-spec error_access_denied(json_rpc_id(), binary()) -> binary().
error_access_denied(Id, Resource) when is_binary(Resource) ->
    Data = #{<<"resource">> => Resource},
    encode_error_response(Id, ?MCP_ERROR_ACCESS_DENIED, ?MCP_MSG_ACCESS_DENIED, Data).

%%====================================================================
%% Protocol Error Helper Functions (-32061 to -32070)
%%====================================================================

%% Unsupported protocol version error
-spec error_unsupported_protocol_version(json_rpc_id(), binary()) -> binary().
error_unsupported_protocol_version(Id, Version) when is_binary(Version) ->
    Data = #{<<"version">> => Version},
    encode_error_response(Id,
                          ?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION,
                          ?MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION,
                          Data).

%% Protocol version mismatch error
-spec error_protocol_version_mismatch(json_rpc_id(), binary(), binary()) -> binary().
error_protocol_version_mismatch(Id, ClientVersion, ServerVersion)
    when is_binary(ClientVersion), is_binary(ServerVersion) ->
    Data = #{<<"clientVersion">> => ClientVersion, <<"serverVersion">> => ServerVersion},
    encode_error_response(Id,
                          ?MCP_ERROR_PROTOCOL_VERSION_MISMATCH,
                          ?MCP_MSG_PROTOCOL_VERSION_MISMATCH,
                          Data).

%% Capability negotiation failed error
-spec error_capability_negotiation_failed(json_rpc_id(), binary()) -> binary().
error_capability_negotiation_failed(Id, Reason) when is_binary(Reason) ->
    Data = #{<<"reason">> => Reason},
    encode_error_response(Id,
                          ?MCP_ERROR_CAPABILITY_NEGOTIATION_FAILED,
                          ?MCP_MSG_CAPABILITY_NEGOTIATION_FAILED,
                          Data).

%% Method not supported error
-spec error_method_not_supported(json_rpc_id(), binary()) -> binary().
error_method_not_supported(Id, Method) when is_binary(Method) ->
    Data = #{<<"method">> => Method},
    encode_error_response(Id, ?MCP_ERROR_METHOD_NOT_SUPPORTED, ?MCP_MSG_METHOD_NOT_SUPPORTED, Data).

%%====================================================================
%% Pagination Error Helper Functions (-32071 to -32080)
%%====================================================================

%% Invalid cursor error
-spec error_invalid_cursor(json_rpc_id(), binary()) -> binary().
error_invalid_cursor(Id, Cursor) when is_binary(Cursor) ->
    Data = #{<<"cursor">> => Cursor},
    encode_error_response(Id, ?MCP_ERROR_INVALID_CURSOR, ?MCP_MSG_INVALID_CURSOR, Data).

%% Cursor expired error
-spec error_cursor_expired(json_rpc_id(), binary()) -> binary().
error_cursor_expired(Id, Cursor) when is_binary(Cursor) ->
    Data = #{<<"cursor">> => Cursor},
    encode_error_response(Id, ?MCP_ERROR_CURSOR_EXPIRED, ?MCP_MSG_CURSOR_EXPIRED, Data).

%% Pagination not supported error
-spec error_pagination_not_supported(json_rpc_id(), binary()) -> binary().
error_pagination_not_supported(Id, Method) when is_binary(Method) ->
    Data = #{<<"method">> => Method},
    encode_error_response(Id,
                          ?MCP_ERROR_PAGINATION_NOT_SUPPORTED,
                          ?MCP_MSG_PAGINATION_NOT_SUPPORTED,
                          Data).

%% Page size too large error
-spec error_page_size_too_large(json_rpc_id(), pos_integer(), pos_integer()) -> binary().
error_page_size_too_large(Id, ActualSize, MaxSize)
    when is_integer(ActualSize), is_integer(MaxSize) ->
    Data = #{<<"actualSize">> => ActualSize, <<"maxSize">> => MaxSize},
    encode_error_response(Id, ?MCP_ERROR_PAGE_SIZE_TOO_LARGE, ?MCP_MSG_PAGE_SIZE_TOO_LARGE, Data).

%%====================================================================
%% Task Error Helper Functions (-32081 to -32090)
%%====================================================================

%% Task not found error
-spec error_task_not_found(json_rpc_id(), binary()) -> binary().
error_task_not_found(Id, TaskId) when is_binary(TaskId) ->
    Data = #{<<"taskId">> => TaskId},
    encode_error_response(Id, ?MCP_ERROR_TASK_NOT_FOUND, ?MCP_MSG_TASK_NOT_FOUND, Data).

%% Task already exists error
-spec error_task_already_exists(json_rpc_id(), binary()) -> binary().
error_task_already_exists(Id, TaskId) when is_binary(TaskId) ->
    Data = #{<<"taskId">> => TaskId},
    encode_error_response(Id, ?MCP_ERROR_TASK_ALREADY_EXISTS, ?MCP_MSG_TASK_ALREADY_EXISTS, Data).

%% Task failed error
-spec error_task_failed(json_rpc_id(), binary(), binary()) -> binary().
error_task_failed(Id, TaskId, Reason) when is_binary(TaskId), is_binary(Reason) ->
    Data = #{<<"taskId">> => TaskId, <<"reason">> => Reason},
    encode_error_response(Id, ?MCP_ERROR_TASK_FAILED, ?MCP_MSG_TASK_FAILED, Data).

%% Task cancelled error
-spec error_task_cancelled(json_rpc_id(), binary()) -> binary().
error_task_cancelled(Id, TaskId) when is_binary(TaskId) ->
    Data = #{<<"taskId">> => TaskId},
    encode_error_response(Id, ?MCP_ERROR_TASK_CANCELLED, ?MCP_MSG_TASK_CANCELLED, Data).

%% Task timeout error
-spec error_task_timeout(json_rpc_id(), binary(), pos_integer()) -> binary().
error_task_timeout(Id, TaskId, TimeoutMs) when is_binary(TaskId), is_integer(TimeoutMs) ->
    Data = #{<<"taskId">> => TaskId, <<"timeoutMs">> => TimeoutMs},
    encode_error_response(Id, ?MCP_ERROR_TASK_TIMEOUT, ?MCP_MSG_TASK_TIMEOUT, Data).

%%====================================================================
%% Progress Error Helper Functions (-32091 to -32100)
%%====================================================================

%% Invalid progress token error
-spec error_invalid_progress_token(json_rpc_id(), binary() | integer()) -> binary().
error_invalid_progress_token(Id, Token) when is_binary(Token); is_integer(Token) ->
    Data = #{<<"progressToken">> => Token},
    encode_error_response(Id,
                          ?MCP_ERROR_INVALID_PROGRESS_TOKEN,
                          ?MCP_MSG_INVALID_PROGRESS_TOKEN,
                          Data).

%% Progress token expired error
-spec error_progress_token_expired(json_rpc_id(), binary() | integer()) -> binary().
error_progress_token_expired(Id, Token) when is_binary(Token); is_integer(Token) ->
    Data = #{<<"progressToken">> => Token},
    encode_error_response(Id,
                          ?MCP_ERROR_PROGRESS_TOKEN_EXPIRED,
                          ?MCP_MSG_PROGRESS_TOKEN_EXPIRED,
                          Data).

%% Progress update failed error
-spec error_progress_update_failed(json_rpc_id(), binary() | integer(), binary()) -> binary().
error_progress_update_failed(Id, Token, Reason)
    when is_binary(Token); is_integer(Token), is_binary(Reason) ->
    Data = #{<<"progressToken">> => Token, <<"reason">> => Reason},
    encode_error_response(Id,
                          ?MCP_ERROR_PROGRESS_UPDATE_FAILED,
                          ?MCP_MSG_PROGRESS_UPDATE_FAILED,
                          Data).

%% Notification failed error
-spec error_notification_failed(json_rpc_id(), binary(), binary()) -> binary().
error_notification_failed(Id, NotificationType, Reason)
    when is_binary(NotificationType), is_binary(Reason) ->
    Data = #{<<"notificationType">> => NotificationType, <<"reason">> => Reason},
    encode_error_response(Id, ?MCP_ERROR_NOTIFICATION_FAILED, ?MCP_MSG_NOTIFICATION_FAILED, Data).

%% Notification queue full error
-spec error_notification_queue_full(json_rpc_id(), pos_integer()) -> binary().
error_notification_queue_full(Id, QueueSize) when is_integer(QueueSize) ->
    Data = #{<<"queueSize">> => QueueSize},
    encode_error_response(Id,
                          ?MCP_ERROR_NOTIFICATION_QUEUE_FULL,
                          ?MCP_MSG_NOTIFICATION_QUEUE_FULL,
                          Data).

%%====================================================================
%% Completion Error Helper Functions (-32110 to -32113)
%%====================================================================

%% Completion not found error
-spec error_completion_not_found(json_rpc_id(), binary()) -> binary().
error_completion_not_found(Id, CompletionId) when is_binary(CompletionId) ->
    Data = #{<<"completionId">> => CompletionId},
    encode_error_response(Id, ?MCP_ERROR_COMPLETION_NOT_FOUND, ?MCP_MSG_COMPLETION_NOT_FOUND, Data).

%% Invalid completion reference error
-spec error_invalid_completion_reference(json_rpc_id(), binary()) -> binary().
error_invalid_completion_reference(Id, Reference) when is_binary(Reference) ->
    Data = #{<<"reference">> => Reference},
    encode_error_response(Id,
                          ?MCP_ERROR_INVALID_COMPLETION_REFERENCE,
                          ?MCP_MSG_INVALID_COMPLETION_REFERENCE,
                          Data).

%% Invalid completion argument error
-spec error_invalid_completion_argument(json_rpc_id(), binary(), binary()) -> binary().
error_invalid_completion_argument(Id, Argument, Details)
    when is_binary(Argument), is_binary(Details) ->
    Data = #{<<"argument">> => Argument, <<"details">> => Details},
    encode_error_response(Id,
                          ?MCP_ERROR_INVALID_COMPLETION_ARGUMENT,
                          ?MCP_MSG_INVALID_COMPLETION_ARGUMENT,
                          Data).

%% Completion failed error
-spec error_completion_failed(json_rpc_id(), binary(), binary()) -> binary().
error_completion_failed(Id, CompletionId, Reason) when is_binary(CompletionId), is_binary(Reason) ->
    Data = #{<<"completionId">> => CompletionId, <<"reason">> => Reason},
    encode_error_response(Id, ?MCP_ERROR_COMPLETION_FAILED, ?MCP_MSG_COMPLETION_FAILED, Data).

%%====================================================================
%% Batch Error Response Functions
%%====================================================================

%% @doc Create error response for invalid batch request
%% Per JSON-RPC 2.0 spec, each invalid request in a batch should generate
%% an error response with the request's ID (if present) or null
-spec create_batch_error_response(map() | term(), atom(), term()) -> json_rpc_message().
create_batch_error_response(Request, Reason, Details) when is_map(Request) ->
    %% Try to extract ID from the request for the error response
    Id = case maps:get(<<"id">>, Request, undefined) of
             undefined ->
                 null;
             IdVal ->
                 IdVal
         end,
    %% Map error reason to JSON-RPC error codes
    {Code, Message} = map_batch_error_to_code(Reason, Details),
    Error = build_error_object(Code, Message, Details),
    #json_rpc_response{id = Id, error = Error};
create_batch_error_response(_Request, Reason, Details) ->
    %% Completely invalid request (not a map), use null ID
    {Code, Message} = map_batch_error_to_code(Reason, Details),
    Error = build_error_object(Code, Message, Details),
    #json_rpc_response{id = null, error = Error}.

%% @doc Map batch error reasons to JSON-RPC error codes and messages
-spec map_batch_error_to_code(atom(), term()) -> {integer(), binary()}.
map_batch_error_to_code(invalid_request, not_an_object) ->
    {?JSONRPC_INVALID_REQUEST, <<"Invalid Request: not an object">>};
map_batch_error_to_code(invalid_request, _) ->
    {?JSONRPC_INVALID_REQUEST, ?JSONRPC_MSG_INVALID_REQUEST};
map_batch_error_to_code(parse_error, _) ->
    {?JSONRPC_PARSE_ERROR, ?JSONRPC_MSG_PARSE_ERROR};
map_batch_error_to_code(missing_jsonrpc, _) ->
    {?JSONRPC_INVALID_REQUEST, <<"Missing jsonrpc version field">>};
map_batch_error_to_code(wrong_version, _) ->
    {?JSONRPC_INVALID_REQUEST, <<"Invalid jsonrpc version (must be 2.0)">>};
map_batch_error_to_code(invalid_method, Method) ->
    Message = <<"Invalid method: ", (binify(Method))/binary>>,
    {?JSONRPC_INVALID_REQUEST, Message};
map_batch_error_to_code(unknown_message_type, _) ->
    {?JSONRPC_INVALID_REQUEST, <<"Unknown message type">>};
map_batch_error_to_code(Reason, _) ->
    %% Fallback to internal error for unknown reasons
    logger:warning("Unknown batch error reason: ~p", [Reason]),
    {?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR}.

%% @doc Convert various types to binary for error messages
-spec binify(term()) -> binary().
binify(Bin) when is_binary(Bin) ->
    Bin;
binify(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
binify(Int) when is_integer(Int) ->
    integer_to_binary(Int);
binify(List) when is_list(List) ->
    list_to_binary(List);
binify(Term) ->
    term_to_binary(Term).

%%====================================================================
%% Batch Processing Functions
%%====================================================================

-spec parse_batch(list()) -> batch_decode_result().
parse_batch([]) ->
    %% Empty batch is invalid per JSON-RPC 2.0 spec
    {error, {invalid_request, empty_batch}};
parse_batch(Requests) when is_list(Requests) ->
    %% Validate version field for all requests first
    case validate_batch_version(Requests) of
        ok ->
            %% Process each request in the batch
            case parse_batch_requests(Requests, []) of
                {ok, Messages} ->
                    {ok, Messages};
                Error ->
                    Error
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate jsonrpc version field in all batch requests
%% Per JSON-RPC 2.0 spec, all requests must have "jsonrpc": "2.0"
-spec validate_batch_version(list()) -> ok | {error, {invalid_request, term()}}.
validate_batch_version([]) ->
    ok;
validate_batch_version([Request | Rest]) when is_map(Request) ->
    case validate_single_request_version(Request) of
        ok ->
            validate_batch_version(Rest);
        Error ->
            Error
    end;
validate_batch_version([_ | _]) ->
    %% Not a map - invalid request structure
    {error, {invalid_request, not_an_object}}.

%% @doc Validate jsonrpc version field in a single request
-spec validate_single_request_version(map()) -> ok | {error, {invalid_request, term()}}.
validate_single_request_version(#{?JSONRPC_FIELD_JSONRPC := ?JSONRPC_VERSION}) ->
    ok;
validate_single_request_version(#{?JSONRPC_FIELD_JSONRPC := Version}) ->
    {error, {invalid_request, {wrong_version, Version}}};
validate_single_request_version(_) ->
    {error, {invalid_request, missing_jsonrpc}}.

-spec parse_batch_requests(list(), [json_rpc_message()]) ->
                              {ok, [json_rpc_message()]} | {error, {atom(), term()}}.
parse_batch_requests([], Acc) ->
    %% All requests processed successfully, return in original order
    {ok, lists:reverse(Acc)};
parse_batch_requests([Request | Rest], Acc) when is_map(Request) ->
    case erlmcp_message_parser:parse_json_rpc(Request) of
        {ok, Message} ->
            parse_batch_requests(Rest, [Message | Acc]);
        {error, {Reason, Details}} ->
            %% Create error response for invalid batch request per JSON-RPC 2.0 spec
            ErrorMsg = create_batch_error_response(Request, Reason, Details),
            parse_batch_requests(Rest, [ErrorMsg | Acc])
    end;
parse_batch_requests([Invalid | Rest], Acc) ->
    %% Completely invalid request (not a map), create error response
    ErrorMsg = create_batch_error_response(Invalid, invalid_request, not_an_object),
    parse_batch_requests(Rest, [ErrorMsg | Acc]).

%%====================================================================
%% Safe Extraction Helper Functions
%%====================================================================

%% @doc Safely extract result from JSON-RPC response wrapper
%% Returns {ok, Result} or {error, invalid_response}
%%
%% Example:
%%   > Response = #{<<"result">> => #{<<"data">> => <<"value">>}, <<"id">> => 1}.
%%   > erlmcp_json_rpc:safe_extract_result(Response).
%%   {ok, #{<<"data">> => <<"value">>}}
-spec safe_extract_result(map() | {ok, map()}) -> {ok, term()} | {error, invalid_response}.
safe_extract_result(Response) ->
    extract_rpc_response(Response).

%% @doc Safely extract error from JSON-RPC response
%% Returns {ok, ErrorMap} or {error, invalid_response}
-spec safe_extract_error(map() | {ok, map()}) -> {ok, map()} | {error, invalid_response}.
safe_extract_error(Response) ->
    extract_rpc_error(Response).

%% @doc Safely extract nested values from response structures
%% Returns {ok, Value} or {ok, Default} if path not found
%%
%% Example:
%%   > Response = #{<<"result">> => #{<<"tool">> => #{<<"name">> => <<"test">>}}}.
%%   > erlmcp_json_rpc:safe_extract_nested(Response, [<<"result">>, <<"tool">>, <<"name">>], undefined).
%%   {ok, <<"test">>}
-spec safe_extract_nested(map() | {ok, map()}, [binary() | atom() | integer()], term()) ->
    {ok, term()} | {error, term()}.
safe_extract_nested({ok, Response}, Path, Default) when is_map(Response) ->
    extract_nested(Response, Path, Default);
safe_extract_nested(Response, Path, Default) when is_map(Response) ->
    extract_nested(Response, Path, Default);
safe_extract_nested(_, _, Default) ->
    {ok, Default}.

%% @doc Safely get field from map with fallback to undefined
%% Returns Value or undefined if not found
%%
%% Example:
%%   > Data = #{<<"key">> => <<"value">>}.
%%   > erlmcp_json_rpc:safe_get_field(Data, <<"key">>).
%%   <<"value">>
-spec safe_get_field(map(), binary() | atom()) -> term() | undefined.
safe_get_field(Map, Key) when is_map(Map) ->
    case get_in(Map, [Key]) of
        {ok, Value} -> Value;
        {error, _} -> undefined
    end;
safe_get_field(_, _) ->
    undefined.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec encode_message(json_rpc_message()) -> binary().
encode_message(Message) ->
    Map = build_message_map(Message),
    erlmcp_json_native:encode(Map).

-spec build_message_map(json_rpc_message()) -> map().
build_message_map(#json_rpc_request{id = Id,
                                    method = Method,
                                    params = Params}) ->
    Base =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => encode_id(Id),
          ?JSONRPC_FIELD_METHOD => Method},
    maybe_add_params(Base, Params);
build_message_map(#json_rpc_response{id = Id,
                                     result = Result,
                                     error = Error}) ->
    Base = #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION, ?JSONRPC_FIELD_ID => encode_id(Id)},
    add_result_or_error(Base, Result, Error);
build_message_map(#json_rpc_notification{method = Method, params = Params}) ->
    Base = #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION, ?JSONRPC_FIELD_METHOD => Method},
    maybe_add_params(Base, Params).

-spec encode_id(json_rpc_id()) -> json_rpc_id().
encode_id(null) ->
    null;
encode_id(Id) when is_binary(Id) ->
    Id;
encode_id(Id) when is_integer(Id) ->
    Id.

-spec maybe_add_params(map(), json_rpc_params()) -> map().
maybe_add_params(Map, undefined) ->
    Map;
maybe_add_params(Map, Params) ->
    Map#{?JSONRPC_FIELD_PARAMS => Params}.

-spec add_result_or_error(map(), term(), map() | undefined) -> map().
add_result_or_error(Map, _Result, Error) when is_map(Error) ->
    Map#{?JSONRPC_FIELD_ERROR => Error};
add_result_or_error(Map, Result, undefined) ->
    Map#{?JSONRPC_FIELD_RESULT => Result}.

-spec build_error_object(integer(), binary(), term() | undefined) -> map().
build_error_object(Code, Message, undefined) ->
    #{?JSONRPC_ERROR_FIELD_CODE => Code, ?JSONRPC_ERROR_FIELD_MESSAGE => Message};
build_error_object(Code, Message, null) ->
    %% Explicit null, don't include data field
    #{?JSONRPC_ERROR_FIELD_CODE => Code, ?JSONRPC_ERROR_FIELD_MESSAGE => Message};
build_error_object(Code, Message, Data) when is_map(Data) ->
    %% Valid map data, include it
    #{?JSONRPC_ERROR_FIELD_CODE => Code,
      ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
      ?JSONRPC_ERROR_FIELD_DATA => Data};
build_error_object(Code, Message, Data) when is_binary(Data) ->
    %% Binary data, wrap in details field
    #{?JSONRPC_ERROR_FIELD_CODE => Code,
      ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
      ?JSONRPC_ERROR_FIELD_DATA => #{<<"details">> => Data}};
build_error_object(Code, Message, Data) ->
    %% Other data types - convert to binary and wrap in details
    DataBin = erlang:term_to_binary(Data),
    #{?JSONRPC_ERROR_FIELD_CODE => Code,
      ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
      ?JSONRPC_ERROR_FIELD_DATA => #{<<"details">> => DataBin}}.

%%====================================================================
%% UTF-8 Support Functions
%%====================================================================

%% @doc Validate that a binary is valid UTF-8 encoded text
%% Returns true if valid, false otherwise
%% Uses binary:is_valid_utf8/1 available in OTP 26+
-spec validate_utf8(binary()) -> boolean().
validate_utf8(Binary) when is_binary(Binary) ->
    %% OTP 26+ has native UTF-8 validation
    try
        binary:is_valid_utf8(Binary)
    catch
        error:undef ->
            %% Fallback for OTP < 26: manual validation
            validate_utf8_fallback(Binary)
    end;
validate_utf8(_) ->
    false.

%% @doc Fallback UTF-8 validation for OTP < 26
%% Checks that the binary contains valid UTF-8 byte sequences
-spec validate_utf8_fallback(binary()) -> boolean().
validate_utf8_fallback(<<>>) ->
    true;
validate_utf8_fallback(<<C, Rest/binary>>) when C =< 127 ->
    %% ASCII (0-127) - always valid
    validate_utf8_fallback(Rest);
validate_utf8_fallback(<<C1, C2, Rest/binary>>) when C1 >= 192, C1 =< 223, C2 >= 128, C2 =< 191 ->
    %% 2-byte sequence
    validate_utf8_fallback(Rest);
validate_utf8_fallback(<<C1, C2, C3, Rest/binary>>) when C1 >= 224, C1 =< 239, C2 >= 128, C2 =< 191, C3 >= 128, C3 =< 191 ->
    %% 3-byte sequence
    validate_utf8_fallback(Rest);
validate_utf8_fallback(<<C1, C2, C3, C4, Rest/binary>>) when C1 >= 240, C1 =< 247, C2 >= 128, C2 =< 191, C3 >= 128, C3 =< 191, C4 >= 128, C4 =< 191 ->
    %% 4-byte sequence
    validate_utf8_fallback(Rest);
validate_utf8_fallback(_) ->
    %% Invalid UTF-8 sequence
    false.

%% @doc Ensure that all binary strings in a term are UTF-8 encoded
%% Validates all binaries in maps, lists, and tuples recursively
%% Returns {ok, ValidatedTerm} or {error, {invalid_utf8, Path}}
-spec ensure_utf8_encoding(term()) -> {ok, term()} | {error, {invalid_utf8, string()}}.
ensure_utf8_encoding(Term) when is_binary(Term) ->
    case validate_utf8(Term) of
        true ->
            {ok, Term};
        false ->
            {error, {invalid_utf8, "binary contains invalid UTF-8 sequences"}}
    end;
ensure_utf8_encoding(Map) when is_map(Map) ->
    ensure_utf8_map(Map, #{}, []);
ensure_utf8_encoding(List) when is_list(List) ->
    ensure_utf8_list(List, [], 1);
ensure_utf8_encoding(Tuple) when is_tuple(Tuple) ->
    ensure_utf8_tuple(Tuple, 1, tuple_size(Tuple));
ensure_utf8_encoding(Term) when is_integer(Term); is_float(Term); is_atom(Term); Term =:= null ->
    %% These types are always valid
    {ok, Term};
ensure_utf8_encoding(_) ->
    {error, {invalid_utf8, "unsupported type"}}.

%% @doc Validate all values in a map
-spec ensure_utf8_map(map(), map(), list()) -> {ok, map()} | {error, {invalid_utf8, string()}}.
ensure_utf8_map(Map, _Acc, _Path) when map_size(Map) =:= 0 ->
    {ok, Map};
ensure_utf8_map(Map, Acc, Path) ->
    maps:fold(fun(K, V, {ok, AccIn}) ->
        PathK = [binary_to_list(K) | Path],
        case ensure_utf8_encoding(K) of
            {ok, _} ->
                case ensure_utf8_encoding(V) of
                    {ok, _} ->
                        {ok, maps:put(K, V, AccIn)};
                    {error, _} = Error ->
                        Error
                end;
            {error, _} = Error ->
                Error
        end
    end, {ok, #{}}, Map).

%% @doc Validate all elements in a list
-spec ensure_utf8_list(list(), list(), integer()) -> {ok, list()} | {error, {invalid_utf8, string()}}.
ensure_utf8_list([], Acc, _Index) ->
    {ok, lists:reverse(Acc)};
ensure_utf8_list([H | T], Acc, Index) ->
    Path = ["[" ++ integer_to_list(Index) ++ "]"],
    case ensure_utf8_encoding(H) of
        {ok, _} ->
            ensure_utf8_list(T, [H | Acc], Index + 1);
        {error, _} = Error ->
            Error
    end.

%% @doc Validate all elements in a tuple
-spec ensure_utf8_tuple(tuple(), integer(), integer()) -> {ok, tuple()} | {error, {invalid_utf8, string()}}.
ensure_utf8_tuple(Tuple, Index, Size) when Index > Size ->
    {ok, Tuple};
ensure_utf8_tuple(Tuple, Index, Size) ->
    Path = ["tuple[" ++ integer_to_list(Index) ++ "]"],
    Element = element(Index, Tuple),
    case ensure_utf8_encoding(Element) of
        {ok, _} ->
            ensure_utf8_tuple(Tuple, Index + 1, Size);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Encoding Functions with UTF-8 Validation
%%====================================================================

%% @doc Encode request with UTF-8 validation
%% Validates that all binary fields are valid UTF-8 before encoding
-spec encode_request_utf8(json_rpc_id(), binary(), json_rpc_params()) ->
    {ok, binary()} | {error, {invalid_utf8, string()}}.
encode_request_utf8(Id, Method, Params) ->
    case ensure_utf8_encoding(Method) of
        {ok, _} ->
            case ensure_utf8_encoding(Params) of
                {ok, _} ->
                    {ok, encode_request(Id, Method, Params)};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Encode response with UTF-8 validation
-spec encode_response_utf8(json_rpc_id(), term()) ->
    {ok, binary()} | {error, {invalid_utf8, string()}}.
encode_response_utf8(Id, Result) ->
    case ensure_utf8_encoding(Result) of
        {ok, _} ->
            {ok, encode_response(Id, Result)};
        {error, _} = Error ->
            Error
    end.

%% Parsing functions moved to erlmcp_message_parser.erl for hot path optimization
%% Import directly from erlmcp_message_parser module
%% Commented out: function is unused, import directly from erlmcp_message_parser
%% parse_json_rpc(Data) ->
%%     erlmcp_message_parser:parse_json_rpc(Data).
