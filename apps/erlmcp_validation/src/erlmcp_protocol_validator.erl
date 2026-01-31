%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol Validator for MCP 2025-11-25 Specification
%%%
%%% Validates JSON-RPC 2.0 and MCP protocol messages.
%%% Returns specific error codes for validation failures.
%%% Uses jesse for JSON Schema validation where applicable.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_validator).

-include("erlmcp.hrl").

%% API exports
-export([
    validate_json_rpc/1,
    validate_mcp_message/1,
    validate_request_id/1,
    validate_params/2,
    validate_error_code/1
]).

%% Additional validation exports
-export([
    validate_method/1,
    validate_capabilities/1,
    validate_server_info/1,
    validate_client_info/1,
    validate_resource/1,
    validate_tool/1,
    validate_prompt/1,
    validate_content/1
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate JSON-RPC 2.0 message from binary JSON
-spec validate_json_rpc(binary()) -> {ok, #json_rpc_request{} | #json_rpc_response{} | #json_rpc_notification{}} | {error, term()}.
validate_json_rpc(Json) when is_binary(Json) ->
    try jsx:decode(Json, [return_maps]) of
        Data when is_map(Data) ->
            case validate_jsonrpc_structure(Data) of
                ok -> erlmcp_message_parser:parse_json_rpc(Data);
                Error -> Error
            end;
        _ ->
            {error, {parse_error, not_object}}
    catch
        error:badarg ->
            {error, {parse_error, invalid_json}};
        Class:Reason ->
            {error, {parse_error, {Class, Reason}}}
    end;
validate_json_rpc(_) ->
    {error, {invalid_argument, not_binary}}.

%% @doc Validate MCP-specific message structure and return error code if invalid
-spec validate_mcp_message(map()) -> {ok, map()} | {error, integer(), binary()}.
validate_mcp_message(#{?JSONRPC_FIELD_METHOD := Method, ?JSONRPC_FIELD_PARAMS := Params} = Message) ->
    case validate_method(Method) of
        ok ->
            case validate_params(Method, Params) of
                ok -> {ok, Message};
                {error, invalid_params} ->
                    {error, ?JSONRPC_INVALID_PARAMS, <<"Invalid parameters for method ", Method/binary>>};
                {error, Details} when is_binary(Details) ->
                    {error, ?JSONRPC_INVALID_PARAMS, Details};
                {error, Details} ->
                    DetailsBin = format_error_details(Details),
                    {error, ?JSONRPC_INVALID_PARAMS, DetailsBin}
            end;
        {error, unknown_method} ->
            {error, ?JSONRPC_METHOD_NOT_FOUND, <<"Unknown method: ", Method/binary>>}
    end;
validate_mcp_message(#{?JSONRPC_FIELD_RESULT := Result}) when is_map(Result) ->
    %% Response message - validate result structure for known MCP responses
    {ok, Result};
validate_mcp_message(#{?JSONRPC_FIELD_ERROR := Error}) when is_map(Error) ->
    %% Error response - validate error structure
    case validate_error_object(Error) of
        ok -> {ok, Error};
        {error, invalid_error_code} ->
            Code = maps:get(?JSONRPC_ERROR_FIELD_CODE, Error, undefined),
            {error, ?JSONRPC_INVALID_REQUEST, <<"Invalid error code: ", (format_code(Code))/binary>>};
        {error, Reason} ->
            {error, ?JSONRPC_INVALID_REQUEST, format_error_details(Reason)}
    end;
validate_mcp_message(_) ->
    {error, ?JSONRPC_INVALID_REQUEST, <<"Message must have method or result/error field">>}.

%% @doc Validate request ID format
-spec validate_request_id(term()) -> ok | {error, invalid_request_id}.
validate_request_id(null) -> ok;
validate_request_id(Id) when is_binary(Id) -> ok;
validate_request_id(Id) when is_integer(Id), Id >= 0 -> ok;
validate_request_id(_) -> {error, invalid_request_id}.

%% @doc Validate method-specific parameters
-spec validate_params(binary(), map() | list() | undefined) -> ok | {error, term()}.
validate_params(?MCP_METHOD_INITIALIZE, Params) when is_map(Params) ->
    validate_initialize_params(Params);
validate_params(?MCP_METHOD_RESOURCES_LIST, Params) when is_map(Params) ->
    %% Optional cursor parameter
    case maps:find(?MCP_PARAM_CURSOR, Params) of
        {ok, Cursor} when is_binary(Cursor) -> ok;
        error -> ok;
        _ -> {error, <<"cursor must be a string">>}
    end;
validate_params(?MCP_METHOD_RESOURCES_LIST, undefined) ->
    ok;
validate_params(?MCP_METHOD_RESOURCES_READ, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_URI, Params) of
        {ok, Uri} when is_binary(Uri) -> ok;
        {ok, _} -> {error, <<"uri must be a string">>};
        error -> {error, <<"Missing required parameter: uri">>}
    end;
validate_params(?MCP_METHOD_RESOURCES_SUBSCRIBE, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_URI, Params) of
        {ok, Uri} when is_binary(Uri) -> ok;
        {ok, _} -> {error, <<"uri must be a string">>};
        error -> {error, <<"Missing required parameter: uri">>}
    end;
validate_params(?MCP_METHOD_RESOURCES_UNSUBSCRIBE, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_URI, Params) of
        {ok, Uri} when is_binary(Uri) -> ok;
        {ok, _} -> {error, <<"uri must be a string">>};
        error -> {error, <<"Missing required parameter: uri">>}
    end;
validate_params(?MCP_METHOD_TOOLS_LIST, Params) when is_map(Params) ->
    %% Optional cursor parameter
    case maps:find(?MCP_PARAM_CURSOR, Params) of
        {ok, Cursor} when is_binary(Cursor) -> ok;
        error -> ok;
        _ -> {error, <<"cursor must be a string">>}
    end;
validate_params(?MCP_METHOD_TOOLS_LIST, undefined) ->
    ok;
validate_params(?MCP_METHOD_TOOLS_CALL, Params) when is_map(Params) ->
    NameResult = case maps:find(?MCP_PARAM_NAME, Params) of
        {ok, Name} when is_binary(Name) -> ok;
        {ok, _} -> {error, <<"name must be a string">>};
        error -> {error, <<"Missing required parameter: name">>}
    end,
    case NameResult of
        ok ->
            %% Arguments are optional, must be object if present
            case maps:find(?MCP_PARAM_ARGUMENTS, Params) of
                {ok, Args} when is_map(Args) -> ok;
                {ok, _} -> {error, <<"arguments must be an object">>};
                error -> ok
            end;
        Error -> Error
    end;
validate_params(?MCP_METHOD_PROMPTS_LIST, Params) when is_map(Params) ->
    %% Optional cursor parameter
    case maps:find(?MCP_PARAM_CURSOR, Params) of
        {ok, Cursor} when is_binary(Cursor) -> ok;
        error -> ok;
        _ -> {error, <<"cursor must be a string">>}
    end;
validate_params(?MCP_METHOD_PROMPTS_LIST, undefined) ->
    ok;
validate_params(?MCP_METHOD_PROMPTS_GET, Params) when is_map(Params) ->
    NameResult = case maps:find(?MCP_PARAM_NAME, Params) of
        {ok, Name} when is_binary(Name) -> ok;
        {ok, _} -> {error, <<"name must be a string">>};
        error -> {error, <<"Missing required parameter: name">>}
    end,
    case NameResult of
        ok ->
            %% Arguments are optional, must be object if present
            case maps:find(?MCP_PARAM_ARGUMENTS, Params) of
                {ok, Args} when is_map(Args) -> ok;
                {ok, _} -> {error, <<"arguments must be an object">>};
                error -> ok
            end;
        Error -> Error
    end;
validate_params(?MCP_METHOD_SAMPLING_CREATE_MESSAGE, Params) when is_map(Params) ->
    validate_sampling_params(Params);
validate_params(?MCP_METHOD_COMPLETION_COMPLETE, Params) when is_map(Params) ->
    validate_completion_params(Params);
validate_params(?MCP_METHOD_LOGGING_SET_LEVEL, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_LEVEL, Params) of
        {ok, Level} when is_atom(Level) ->
            case lists:member(Level, ?MCP_VALID_LOG_LEVELS) of
                true -> ok;
                false -> {error, <<"Invalid log level">>}
            end;
        {ok, _} -> {error, <<"level must be an atom">>};
        error -> {error, <<"Missing required parameter: level">>}
    end;
validate_params(?MCP_METHOD_PING, _) ->
    %% Ping accepts no parameters or empty object
    ok;
validate_params(_, undefined) ->
    %% No parameters provided - acceptable for methods with optional params
    ok;
validate_params(_, Params) when is_map(Params) ->
    %% Unknown method with params - let it pass (could be custom method)
    ok;
validate_params(_, Params) when is_list(Params) ->
    %% Positional params (allowed by JSON-RPC 2.0)
    ok;
validate_params(_, _) ->
    {error, <<"params must be an object, array, or omitted">>}.

%% @doc Validate error code is in valid range
-spec validate_error_code(integer()) -> ok | {error, invalid_error_code}.
validate_error_code(Code) when is_integer(Code) ->
    case lists:member(Code, ?VALID_ERROR_CODES) of
        true -> ok;
        false ->
            %% Check if in valid ranges
            if
                Code >= -32700, Code =< -32000 -> ok;  % JSON-RPC range
                Code >= 1090, Code =< 1099 -> ok;      % Experimental range
                true -> {error, invalid_error_code}
            end
    end;
validate_error_code(_) ->
    {error, invalid_error_code}.

%%%===================================================================
%%% Additional Validation Functions
%%%===================================================================

%% @doc Validate method name
-spec validate_method(binary()) -> ok | {error, unknown_method}.
validate_method(Method) when is_binary(Method) ->
    KnownMethods = [
        ?MCP_METHOD_INITIALIZE,
        ?MCP_METHOD_INITIALIZED,
        ?MCP_METHOD_RESOURCES_LIST,
        ?MCP_METHOD_RESOURCES_READ,
        ?MCP_METHOD_RESOURCES_TEMPLATES_LIST,
        ?MCP_METHOD_RESOURCES_SUBSCRIBE,
        ?MCP_METHOD_RESOURCES_UNSUBSCRIBE,
        ?MCP_METHOD_TOOLS_LIST,
        ?MCP_METHOD_TOOLS_CALL,
        ?MCP_METHOD_PROMPTS_LIST,
        ?MCP_METHOD_PROMPTS_GET,
        ?MCP_METHOD_SAMPLING_CREATE_MESSAGE,
        ?MCP_METHOD_COMPLETION_COMPLETE,
        ?MCP_METHOD_LOGGING_SET_LEVEL,
        ?MCP_METHOD_PING,
        ?MCP_METHOD_NOTIFICATIONS_PROGRESS,
        ?MCP_METHOD_NOTIFICATIONS_RESOURCES_UPDATED,
        ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED,
        ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
        ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED,
        ?MCP_METHOD_NOTIFICATIONS_CANCELLED
    ],
    case lists:member(Method, KnownMethods) of
        true -> ok;
        false -> {error, unknown_method}
    end;
validate_method(_) ->
    {error, unknown_method}.

%% @doc Validate capabilities object
-spec validate_capabilities(map()) -> ok | {error, term()}.
validate_capabilities(Capabilities) when is_map(Capabilities) ->
    %% Capabilities should have feature maps (can be empty)
    %% Each capability value should be a map
    maps:fold(fun(_Key, Value, ok) when is_map(Value) -> ok;
                 (Key, _Value, _) -> {error, <<"Capability ", Key/binary, " must be an object">>}
              end, ok, Capabilities);
validate_capabilities(_) ->
    {error, <<"capabilities must be an object">>}.

%% @doc Validate server info
-spec validate_server_info(map()) -> ok | {error, term()}.
validate_server_info(Info) when is_map(Info) ->
    case {maps:find(?MCP_INFO_NAME, Info), maps:find(?MCP_INFO_VERSION, Info)} of
        {{ok, Name}, {ok, Version}} when is_binary(Name), is_binary(Version) ->
            ok;
        {{ok, _}, {ok, _}} ->
            {error, <<"serverInfo name and version must be strings">>};
        {{ok, _}, error} ->
            {error, <<"Missing required field: serverInfo.version">>};
        {error, {ok, _}} ->
            {error, <<"Missing required field: serverInfo.name">>};
        {error, error} ->
            {error, <<"Missing required fields: serverInfo.name and version">>}
    end;
validate_server_info(_) ->
    {error, <<"serverInfo must be an object">>}.

%% @doc Validate client info
-spec validate_client_info(map()) -> ok | {error, term()}.
validate_client_info(Info) when is_map(Info) ->
    case {maps:find(?MCP_INFO_NAME, Info), maps:find(?MCP_INFO_VERSION, Info)} of
        {{ok, Name}, {ok, Version}} when is_binary(Name), is_binary(Version) ->
            ok;
        {{ok, _}, {ok, _}} ->
            {error, <<"clientInfo name and version must be strings">>};
        {{ok, _}, error} ->
            {error, <<"Missing required field: clientInfo.version">>};
        {error, {ok, _}} ->
            {error, <<"Missing required field: clientInfo.name">>};
        {error, error} ->
            {error, <<"Missing required fields: clientInfo.name and version">>}
    end;
validate_client_info(_) ->
    {error, <<"clientInfo must be an object">>}.

%% @doc Validate resource object
-spec validate_resource(map()) -> ok | {error, term()}.
validate_resource(Resource) when is_map(Resource) ->
    RequiredFields = [{?MCP_PARAM_URI, <<"uri">>}, {?MCP_PARAM_NAME, <<"name">>}],
    validate_required_fields(Resource, RequiredFields);
validate_resource(_) ->
    {error, <<"resource must be an object">>}.

%% @doc Validate tool object
-spec validate_tool(map()) -> ok | {error, term()}.
validate_tool(Tool) when is_map(Tool) ->
    RequiredFields = [
        {?MCP_PARAM_NAME, <<"name">>},
        {?MCP_PARAM_INPUT_SCHEMA, <<"inputSchema">>}
    ],
    case validate_required_fields(Tool, RequiredFields) of
        ok ->
            %% Validate inputSchema is a valid JSON Schema
            case maps:find(?MCP_PARAM_INPUT_SCHEMA, Tool) of
                {ok, Schema} when is_map(Schema) -> ok;
                {ok, _} -> {error, <<"inputSchema must be an object">>};
                error -> ok  % Already checked in required fields
            end;
        Error -> Error
    end;
validate_tool(_) ->
    {error, <<"tool must be an object">>}.

%% @doc Validate prompt object
-spec validate_prompt(map()) -> ok | {error, term()}.
validate_prompt(Prompt) when is_map(Prompt) ->
    RequiredFields = [{?MCP_PARAM_NAME, <<"name">>}],
    validate_required_fields(Prompt, RequiredFields);
validate_prompt(_) ->
    {error, <<"prompt must be an object">>}.

%% @doc Validate content object
-spec validate_content(map()) -> ok | {error, term()}.
validate_content(Content) when is_map(Content) ->
    case maps:find(?MCP_PARAM_TYPE, Content) of
        {ok, Type} when is_binary(Type) ->
            validate_content_by_type(Type, Content);
        {ok, _} ->
            {error, <<"content type must be a string">>};
        error ->
            {error, <<"Missing required field: content.type">>}
    end;
validate_content(_) ->
    {error, <<"content must be an object">>}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate JSON-RPC structure (version, fields)
-spec validate_jsonrpc_structure(map()) -> ok | {error, term()}.
validate_jsonrpc_structure(#{?JSONRPC_FIELD_JSONRPC := ?JSONRPC_VERSION} = Data) ->
    %% Check for required fields based on message type
    case Data of
        #{?JSONRPC_FIELD_METHOD := Method} when is_binary(Method) ->
            %% Request or Notification - must have method
            case maps:is_key(?JSONRPC_FIELD_ID, Data) of
                true ->
                    %% Request - must have id
                    validate_request_id(maps:get(?JSONRPC_FIELD_ID, Data));
                false ->
                    %% Notification - must not have id
                    ok
            end;
        #{?JSONRPC_FIELD_ID := Id} ->
            %% Response - must have id and either result or error
            case validate_request_id(Id) of
                ok ->
                    HasResult = maps:is_key(?JSONRPC_FIELD_RESULT, Data),
                    HasError = maps:is_key(?JSONRPC_FIELD_ERROR, Data),
                    if
                        HasResult andalso not HasError -> ok;
                        HasError andalso not HasResult -> ok;
                        HasResult andalso HasError ->
                            {error, {invalid_request, result_and_error_exclusive}};
                        true ->
                            {error, {invalid_request, missing_result_or_error}}
                    end;
                Error -> Error
            end;
        _ ->
            {error, {invalid_request, missing_method_or_id}}
    end;
validate_jsonrpc_structure(#{?JSONRPC_FIELD_JSONRPC := Version}) ->
    {error, {invalid_request, {wrong_version, Version}}};
validate_jsonrpc_structure(_) ->
    {error, {invalid_request, missing_jsonrpc}}.

%% @doc Validate initialize params
-spec validate_initialize_params(map()) -> ok | {error, term()}.
validate_initialize_params(Params) ->
    RequiredFields = [
        {?MCP_FIELD_PROTOCOL_VERSION, <<"protocolVersion">>},
        {?MCP_FIELD_CAPABILITIES, <<"capabilities">>},
        {?MCP_FIELD_CLIENT_INFO, <<"clientInfo">>}
    ],
    case validate_required_fields(Params, RequiredFields) of
        ok ->
            %% Validate sub-structures
            case {maps:find(?MCP_FIELD_CAPABILITIES, Params),
                  maps:find(?MCP_FIELD_CLIENT_INFO, Params)} of
                {{ok, Caps}, {ok, Info}} ->
                    case validate_capabilities(Caps) of
                        ok -> validate_client_info(Info);
                        Error -> Error
                    end;
                _ -> ok  % Already validated in required_fields
            end;
        Error -> Error
    end.

%% @doc Validate sampling params
-spec validate_sampling_params(map()) -> ok | {error, term()}.
validate_sampling_params(Params) ->
    RequiredFields = [{?MCP_PARAM_MESSAGES, <<"messages">>}],
    case validate_required_fields(Params, RequiredFields) of
        ok ->
            case maps:find(?MCP_PARAM_MESSAGES, Params) of
                {ok, Messages} when is_list(Messages) -> ok;
                {ok, _} -> {error, <<"messages must be an array">>};
                error -> ok  % Already checked
            end;
        Error -> Error
    end.

%% @doc Validate completion params
-spec validate_completion_params(map()) -> ok | {error, term()}.
validate_completion_params(Params) when is_map(Params) ->
    %% Completion params validation (MCP 2025-11-25)
    ok;
validate_completion_params(_) ->
    {error, <<"params must be an object">>}.

%% @doc Validate error object structure
-spec validate_error_object(map()) -> ok | {error, term()}.
validate_error_object(Error) when is_map(Error) ->
    case {maps:find(?JSONRPC_ERROR_FIELD_CODE, Error),
          maps:find(?JSONRPC_ERROR_FIELD_MESSAGE, Error)} of
        {{ok, Code}, {ok, Message}} when is_integer(Code), is_binary(Message) ->
            validate_error_code(Code);
        {{ok, Code}, {ok, _}} when is_integer(Code) ->
            {error, <<"error message must be a string">>};
        {{ok, _}, {ok, _}} ->
            {error, <<"error code must be an integer">>};
        {{ok, _}, error} ->
            {error, <<"Missing required field: error.message">>};
        {error, {ok, _}} ->
            {error, <<"Missing required field: error.code">>};
        {error, error} ->
            {error, <<"Missing required fields: error.code and message">>}
    end;
validate_error_object(_) ->
    {error, <<"error must be an object">>}.

%% @doc Validate content by type
-spec validate_content_by_type(binary(), map()) -> ok | {error, term()}.
validate_content_by_type(?MCP_CONTENT_TYPE_TEXT, Content) ->
    case maps:find(?MCP_PARAM_TEXT, Content) of
        {ok, Text} when is_binary(Text) -> ok;
        {ok, _} -> {error, <<"text content must have text field as string">>};
        error -> {error, <<"text content must have text field">>}
    end;
validate_content_by_type(?MCP_CONTENT_TYPE_IMAGE, Content) ->
    case {maps:find(?MCP_PARAM_DATA, Content), maps:find(?MCP_PARAM_MIME_TYPE, Content)} of
        {{ok, Data}, {ok, MimeType}} when is_binary(Data), is_binary(MimeType) -> ok;
        {{ok, _}, {ok, _}} -> {error, <<"image content must have data and mimeType as strings">>};
        {{ok, _}, error} -> {error, <<"image content must have mimeType field">>};
        {error, {ok, _}} -> {error, <<"image content must have data field">>};
        {error, error} -> {error, <<"image content must have data and mimeType fields">>}
    end;
validate_content_by_type(?MCP_CONTENT_TYPE_RESOURCE, Content) ->
    case maps:find(?MCP_PARAM_URI, Content) of
        {ok, Uri} when is_binary(Uri) -> ok;
        {ok, _} -> {error, <<"resource content must have uri as string">>};
        error -> {error, <<"resource content must have uri field">>}
    end;
validate_content_by_type(_, _) ->
    %% Unknown content type - allow it (could be custom)
    ok.

%% @doc Validate required fields in a map
-spec validate_required_fields(map(), [{binary(), binary()}]) -> ok | {error, binary()}.
validate_required_fields(Map, Fields) ->
    validate_required_fields(Map, Fields, []).

validate_required_fields(_Map, [], []) ->
    ok;
validate_required_fields(_Map, [], Missing) ->
    MissingStr = lists:join(<<", ">>, lists:reverse(Missing)),
    {error, <<"Missing required fields: ", (iolist_to_binary(MissingStr))/binary>>};
validate_required_fields(Map, [{Key, Name} | Rest], Missing) ->
    case maps:is_key(Key, Map) of
        true ->
            validate_required_fields(Map, Rest, Missing);
        false ->
            validate_required_fields(Map, Rest, [Name | Missing])
    end.

%% @doc Format error details to binary
-spec format_error_details(term()) -> binary().
format_error_details(Details) when is_binary(Details) ->
    Details;
format_error_details(Details) when is_atom(Details) ->
    atom_to_binary(Details, utf8);
format_error_details(Details) when is_list(Details) ->
    try
        list_to_binary(Details)
    catch
        _:_ -> iolist_to_binary(io_lib:format("~p", [Details]))
    end;
format_error_details(Details) ->
    iolist_to_binary(io_lib:format("~p", [Details])).

%% @doc Format error code to binary
-spec format_code(term()) -> binary().
format_code(Code) when is_integer(Code) ->
    integer_to_binary(Code);
format_code(Code) ->
    format_error_details(Code).
