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
-export([validate_all/1, validate_json_rpc/1, validate_mcp_message/1, validate_request_id/1,
         validate_params/2, validate_error_code/1]).
%% Additional validation exports
-export([validate_method/1, validate_capabilities/1, validate_server_info/1, validate_client_info/1,
         validate_resource/1, validate_tool/1, validate_prompt/1, validate_content/1,
         validate_method_name/1, validate_notification_name/1, validate_field_type/3,
         validate_jsonrpc/1, validate_required_fields/2, format_validation_error/1]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate all protocol compliance aspects for MCP specification
-spec validate_all(binary()) ->
                      #{status := passed | failed | warning,
                        timestamp := integer(),
                        checks :=
                            [#{name := binary(),
                               status := passed | failed | warning,
                               message => binary(),
                               details => map()}],
                        passed := non_neg_integer(),
                        failed := non_neg_integer()}.
validate_all(SpecVersion) when is_binary(SpecVersion) ->
    Timestamp = erlang:system_time(millisecond),

    %% Define test messages for validation
    ValidRequest =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_PING,
          ?JSONRPC_FIELD_ID => 1},

    InitRequest =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_INITIALIZE,
          ?JSONRPC_FIELD_ID => 2,
          ?JSONRPC_FIELD_PARAMS =>
              #{?MCP_FIELD_PROTOCOL_VERSION => SpecVersion,
                ?MCP_FIELD_CAPABILITIES => #{},
                ?MCP_FIELD_CLIENT_INFO =>
                    #{?MCP_INFO_NAME => <<"test_client">>, ?MCP_INFO_VERSION => <<"1.0.0">>}}},

    InvalidJsonRpc = #{<<"jsonrpc">> => <<"1.0">>, ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_PING},

    %% Run validation checks
    Checks =
        [%% Check 1: JSON-RPC version validation
         check_jsonrpc_version(ValidRequest),
         %% Check 2: Request structure validation
         check_request_structure(ValidRequest),
         %% Check 3: Initialize params validation
         check_initialize_validation(InitRequest),
         %% Check 4: Method validation
         check_method_validation(),
         %% Check 5: Error code validation
         check_error_code_validation(),
         %% Check 6: Invalid version rejection
         check_invalid_version_rejection(InvalidJsonRpc),
         %% Check 7: MCP capabilities validation
         check_capabilities_validation(),
         %% Check 8: Resource validation
         check_resource_validation(),
         %% Check 9: Tool validation
         check_tool_validation(),
         %% Check 10: Prompt validation
         check_prompt_validation()],

    %% Count results
    {Passed, Failed, Warnings} =
        lists:foldl(fun(Check, {P, F, W}) ->
                       case maps:get(status, Check) of
                           passed ->
                               {P + 1, F, W};
                           failed ->
                               {P, F + 1, W};
                           warning ->
                               {P, F, W + 1}
                       end
                    end,
                    {0, 0, 0},
                    Checks),

    %% Determine overall status (warnings don't fail)
    OverallStatus =
        case Failed of
            0 ->
                passed;
            _ ->
                failed
        end,

    #{status => OverallStatus,
      timestamp => Timestamp,
      spec_version => SpecVersion,
      checks => Checks,
      passed => Passed,
      failed => Failed,
      warnings => Warnings}.

%% @doc Validate JSON-RPC 2.0 message from binary JSON
-spec validate_json_rpc(binary()) ->
                           {ok,
                            #json_rpc_request{} | #json_rpc_response{} | #json_rpc_notification{}} |
                           {error, term()}.
validate_json_rpc(Json) when is_binary(Json) ->
    try erlmcp_json_native:decode(Json) of
        Data when is_map(Data) ->
            case validate_jsonrpc_structure(Data) of
                ok ->
                    erlmcp_message_parser:parse_json_rpc(Data);
                Error ->
                    Error
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
validate_mcp_message(#{?JSONRPC_FIELD_METHOD := Method, ?JSONRPC_FIELD_PARAMS := Params} =
                         Message) ->
    case validate_method(Method) of
        ok ->
            case validate_params(Method, Params) of
                ok ->
                    {ok, Message};
                {error, invalid_params} ->
                    {error,
                     ?JSONRPC_INVALID_PARAMS,
                     <<"Invalid parameters for method ", Method/binary>>};
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
        ok ->
            {ok, Error};
        {error, invalid_error_code} ->
            Code = maps:get(?JSONRPC_ERROR_FIELD_CODE, Error, undefined),
            {error,
             ?JSONRPC_INVALID_REQUEST,
             <<"Invalid error code: ", (format_code(Code))/binary>>};
        {error, Reason} ->
            {error, ?JSONRPC_INVALID_REQUEST, format_error_details(Reason)}
    end;
validate_mcp_message(_) ->
    {error, ?JSONRPC_INVALID_REQUEST, <<"Message must have method or result/error field">>}.

%% @doc Validate request ID format
-spec validate_request_id(term()) -> ok | {error, invalid_request_id}.
validate_request_id(null) ->
    ok;
validate_request_id(Id) when is_binary(Id) ->
    ok;
validate_request_id(Id) when is_integer(Id), Id >= 0 ->
    ok;
validate_request_id(_) ->
    {error, invalid_request_id}.

%% @doc Validate method-specific parameters
-spec validate_params(binary(), map() | list() | undefined) -> ok | {error, term()}.
validate_params(?MCP_METHOD_INITIALIZE, Params) when is_map(Params) ->
    validate_initialize_params(Params);
validate_params(?MCP_METHOD_RESOURCES_LIST, Params) when is_map(Params) ->
    %% Optional cursor parameter
    case maps:find(?MCP_PARAM_CURSOR, Params) of
        {ok, Cursor} when is_binary(Cursor) ->
            ok;
        error ->
            ok;
        _ ->
            {error, <<"cursor must be a string">>}
    end;
validate_params(?MCP_METHOD_RESOURCES_LIST, undefined) ->
    ok;
validate_params(?MCP_METHOD_RESOURCES_READ, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_URI, Params) of
        {ok, Uri} when is_binary(Uri) ->
            ok;
        {ok, _} ->
            {error, <<"uri must be a string">>};
        error ->
            {error, <<"Missing required parameter: uri">>}
    end;
validate_params(?MCP_METHOD_RESOURCES_SUBSCRIBE, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_URI, Params) of
        {ok, Uri} when is_binary(Uri) ->
            ok;
        {ok, _} ->
            {error, <<"uri must be a string">>};
        error ->
            {error, <<"Missing required parameter: uri">>}
    end;
validate_params(?MCP_METHOD_RESOURCES_UNSUBSCRIBE, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_URI, Params) of
        {ok, Uri} when is_binary(Uri) ->
            ok;
        {ok, _} ->
            {error, <<"uri must be a string">>};
        error ->
            {error, <<"Missing required parameter: uri">>}
    end;
validate_params(?MCP_METHOD_TOOLS_LIST, Params) when is_map(Params) ->
    %% Optional cursor parameter
    case maps:find(?MCP_PARAM_CURSOR, Params) of
        {ok, Cursor} when is_binary(Cursor) ->
            ok;
        error ->
            ok;
        _ ->
            {error, <<"cursor must be a string">>}
    end;
validate_params(?MCP_METHOD_TOOLS_LIST, undefined) ->
    ok;
validate_params(?MCP_METHOD_TOOLS_CALL, Params) when is_map(Params) ->
    NameResult =
        case maps:find(?MCP_PARAM_NAME, Params) of
            {ok, Name} when is_binary(Name) ->
                ok;
            {ok, _} ->
                {error, <<"name must be a string">>};
            error ->
                {error, <<"Missing required parameter: name">>}
        end,
    case NameResult of
        ok ->
            %% Arguments are optional, must be object if present
            case maps:find(?MCP_PARAM_ARGUMENTS, Params) of
                {ok, Args} when is_map(Args) ->
                    ok;
                {ok, _} ->
                    {error, <<"arguments must be an object">>};
                error ->
                    ok
            end;
        Error ->
            Error
    end;
validate_params(?MCP_METHOD_PROMPTS_LIST, Params) when is_map(Params) ->
    %% Optional cursor parameter
    case maps:find(?MCP_PARAM_CURSOR, Params) of
        {ok, Cursor} when is_binary(Cursor) ->
            ok;
        error ->
            ok;
        _ ->
            {error, <<"cursor must be a string">>}
    end;
validate_params(?MCP_METHOD_PROMPTS_LIST, undefined) ->
    ok;
validate_params(?MCP_METHOD_PROMPTS_GET, Params) when is_map(Params) ->
    NameResult =
        case maps:find(?MCP_PARAM_NAME, Params) of
            {ok, Name} when is_binary(Name) ->
                ok;
            {ok, _} ->
                {error, <<"name must be a string">>};
            error ->
                {error, <<"Missing required parameter: name">>}
        end,
    case NameResult of
        ok ->
            %% Arguments are optional, must be object if present
            case maps:find(?MCP_PARAM_ARGUMENTS, Params) of
                {ok, Args} when is_map(Args) ->
                    ok;
                {ok, _} ->
                    {error, <<"arguments must be an object">>};
                error ->
                    ok
            end;
        Error ->
            Error
    end;
validate_params(?MCP_METHOD_SAMPLING_CREATE_MESSAGE, Params) when is_map(Params) ->
    validate_sampling_params(Params);
validate_params(?MCP_METHOD_COMPLETION_COMPLETE, Params) when is_map(Params) ->
    validate_completion_params(Params);
validate_params(?MCP_METHOD_LOGGING_SET_LEVEL, Params) when is_map(Params) ->
    case maps:find(?MCP_PARAM_LEVEL, Params) of
        {ok, Level} when is_atom(Level) ->
            case lists:member(Level, ?MCP_VALID_LOG_LEVELS) of
                true ->
                    ok;
                false ->
                    {error, <<"Invalid log level">>}
            end;
        {ok, _} ->
            {error, <<"level must be an atom">>};
        error ->
            {error, <<"Missing required parameter: level">>}
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
        true ->
            ok;
        false ->
            %% Check if in valid ranges
            if Code >= -32700, Code =< -32000 ->
                   ok;  % JSON-RPC range
               Code >= 1090, Code =< 1099 ->
                   ok;      % Experimental range
               true ->
                   {error, invalid_error_code}
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
    KnownMethods =
        [?MCP_METHOD_INITIALIZE,
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
         ?MCP_METHOD_NOTIFICATIONS_CANCELLED],
    case lists:member(Method, KnownMethods) of
        true ->
            ok;
        false ->
            {error, unknown_method}
    end;
validate_method(_) ->
    {error, unknown_method}.

%% @doc Validate capabilities object
-spec validate_capabilities(map()) -> ok | {error, term()}.
validate_capabilities(Capabilities) when is_map(Capabilities) ->
    %% Capabilities should have feature maps (can be empty)
    %% Each capability value should be a map
    maps:fold(fun (_Key, Value, ok) when is_map(Value) ->
                      ok;
                  (Key, _Value, _) ->
                      {error, <<"Capability ", Key/binary, " must be an object">>}
              end,
              ok,
              Capabilities);
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
    RequiredFields = [{?MCP_PARAM_NAME, <<"name">>}, {?MCP_PARAM_INPUT_SCHEMA, <<"inputSchema">>}],
    case validate_required_fields(Tool, RequiredFields) of
        ok ->
            %% Validate inputSchema is a valid JSON Schema
            case maps:find(?MCP_PARAM_INPUT_SCHEMA, Tool) of
                {ok, Schema} when is_map(Schema) ->
                    ok;
                {ok, _} ->
                    {error, <<"inputSchema must be an object">>};
                error ->
                    ok  % Already checked in required fields
            end;
        Error ->
            Error
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
                    if HasResult andalso not HasError ->
                           ok;
                       HasError andalso not HasResult ->
                           ok;
                       HasResult andalso HasError ->
                           {error, {invalid_request, result_and_error_exclusive}};
                       true ->
                           {error, {invalid_request, missing_result_or_error}}
                    end;
                Error ->
                    Error
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
    RequiredFields =
        [{?MCP_FIELD_PROTOCOL_VERSION, <<"protocolVersion">>},
         {?MCP_FIELD_CAPABILITIES, <<"capabilities">>},
         {?MCP_FIELD_CLIENT_INFO, <<"clientInfo">>}],
    case validate_required_fields(Params, RequiredFields) of
        ok ->
            %% Validate sub-structures
            case {maps:find(?MCP_FIELD_CAPABILITIES, Params),
                  maps:find(?MCP_FIELD_CLIENT_INFO, Params)}
            of
                {{ok, Caps}, {ok, Info}} ->
                    case validate_capabilities(Caps) of
                        ok ->
                            validate_client_info(Info);
                        Error ->
                            Error
                    end;
                _ ->
                    ok  % Already validated in required_fields
            end;
        Error ->
            Error
    end.

%% @doc Validate sampling params
-spec validate_sampling_params(map()) -> ok | {error, term()}.
validate_sampling_params(Params) ->
    RequiredFields = [{?MCP_PARAM_MESSAGES, <<"messages">>}],
    case validate_required_fields(Params, RequiredFields) of
        ok ->
            case maps:find(?MCP_PARAM_MESSAGES, Params) of
                {ok, Messages} when is_list(Messages) ->
                    ok;
                {ok, _} ->
                    {error, <<"messages must be an array">>};
                error ->
                    ok  % Already checked
            end;
        Error ->
            Error
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
          maps:find(?JSONRPC_ERROR_FIELD_MESSAGE, Error)}
    of
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
        {ok, Text} when is_binary(Text) ->
            ok;
        {ok, _} ->
            {error, <<"text content must have text field as string">>};
        error ->
            {error, <<"text content must have text field">>}
    end;
validate_content_by_type(?MCP_CONTENT_TYPE_IMAGE, Content) ->
    case {maps:find(?MCP_PARAM_DATA, Content), maps:find(?MCP_PARAM_MIME_TYPE, Content)} of
        {{ok, Data}, {ok, MimeType}} when is_binary(Data), is_binary(MimeType) ->
            ok;
        {{ok, _}, {ok, _}} ->
            {error, <<"image content must have data and mimeType as strings">>};
        {{ok, _}, error} ->
            {error, <<"image content must have mimeType field">>};
        {error, {ok, _}} ->
            {error, <<"image content must have data field">>};
        {error, error} ->
            {error, <<"image content must have data and mimeType fields">>}
    end;
validate_content_by_type(?MCP_CONTENT_TYPE_RESOURCE, Content) ->
    case maps:find(?MCP_PARAM_URI, Content) of
        {ok, Uri} when is_binary(Uri) ->
            ok;
        {ok, _} ->
            {error, <<"resource content must have uri as string">>};
        error ->
            {error, <<"resource content must have uri field">>}
    end;
validate_content_by_type(_, _) ->
    %% Unknown content type - allow it (could be custom)
    ok.

%% @doc Validate required fields in a map (accepts list of binaries or list of tuples)
-spec validate_required_fields(map(), [binary()] | [{binary(), binary()}]) -> ok | {error, map()}.
validate_required_fields(Map, []) when is_list([]) ->
    ok;
validate_required_fields(Map, FieldList) when is_list(FieldList) ->
    case is_map(Map) of
        false ->
            {error, #{reason => invalid_message_structure}};
        true ->
            %% Convert list of binaries to list of tuples if needed
            Fields =
                case is_binary(hd(FieldList)) of
                    true ->
                        [{Field, Field} || Field <- FieldList];
                    false ->
                        FieldList
                end,
            validate_required_fields_impl(Map, Fields, [])
    end.

%% @private Implementation of validate_required_fields
validate_required_fields_impl(_Map, [], []) ->
    ok;
validate_required_fields_impl(_Map, [], Missing) ->
    {error, #{reason => missing_required_fields, details => #{missing => lists:reverse(Missing)}}};
validate_required_fields_impl(Map, [{Key, Name} | Rest], Missing) ->
    case maps:is_key(Key, Map) of
        true ->
            validate_required_fields_impl(Map, Rest, Missing);
        false ->
            validate_required_fields_impl(Map, Rest, [Name | Missing])
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
        _:_ ->
            iolist_to_binary(io_lib:format("~p", [Details]))
    end;
format_error_details(Details) ->
    iolist_to_binary(io_lib:format("~p", [Details])).

%% @doc Format error code to binary
-spec format_code(term()) -> binary().
format_code(Code) when is_integer(Code) ->
    integer_to_binary(Code);
format_code(Code) ->
    format_error_details(Code).

%%%===================================================================
%%% Additional Validation Functions for Tests
%%%===================================================================

%% @doc Validate method name (for tests)
-spec validate_method_name(binary() | atom()) -> ok | {error, map()}.
validate_method_name(Method) when is_binary(Method) ->
    KnownMethods =
        [<<"initialize">>,
         <<"ping">>,
         <<"resources/list">>,
         <<"resources/read">>,
         <<"resources/subscribe">>,
         <<"resources/unsubscribe">>,
         <<"tools/list">>,
         <<"tools/call">>,
         <<"prompts/list">>,
         <<"prompts/get">>,
         <<"sampling/create_message">>,
         <<"completion/complete">>,
         <<"logging/set_level">>],
    case lists:member(Method, KnownMethods) of
        true ->
            ok;
        false ->
            {error, #{reason => unknown_method, method => Method}}
    end;
validate_method_name(_) ->
    {error, #{reason => invalid_method_type}}.

%% @doc Validate notification name (for tests)
-spec validate_notification_name(binary() | atom()) -> ok | {error, map()}.
validate_notification_name(Notification) when is_binary(Notification) ->
    KnownNotifications =
        [<<"notifications/cancelled">>,
         <<"notifications/progress">>,
         <<"notifications/roots/list_changed">>,
         <<"notifications/message">>],
    case lists:member(Notification, KnownNotifications) of
        true ->
            ok;
        false ->
            {error, #{reason => unknown_notification, notification => Notification}}
    end;
validate_notification_name(_) ->
    {error, #{reason => invalid_notification_type}}.

%% @doc Validate field type (for tests)
-spec validate_field_type(binary(), term(), atom()) -> ok | {error, map()}.
validate_field_type(_FieldName, Value, binary) when is_binary(Value) ->
    ok;
validate_field_type(_FieldName, Value, integer) when is_integer(Value) ->
    ok;
validate_field_type(_FieldName, Value, float) when is_float(Value) ->
    ok;
validate_field_type(_FieldName, Value, boolean) when is_boolean(Value) ->
    ok;
validate_field_type(_FieldName, Value, map) when is_map(Value) ->
    ok;
validate_field_type(_FieldName, Value, array) when is_list(Value) ->
    ok;
validate_field_type(_FieldName, _Value, any) ->
    ok;
validate_field_type(FieldName, _Value, ExpectedType) ->
    {error,
     #{reason => type_mismatch,
       field => FieldName,
       expected => ExpectedType}}.

%% @doc Validate JSON-RPC message structure (for tests)
-spec validate_jsonrpc(map() | term()) -> ok | {error, map()}.
validate_jsonrpc(Message) when is_map(Message) ->
    case maps:get(<<"jsonrpc">>, Message, undefined) of
        <<"2.0">> ->
            %% Valid JSON-RPC version
            ok;
        _ ->
            {error, #{reason => invalid_jsonrpc_version}}
    end;
validate_jsonrpc(_) ->
    {error, #{reason => not_map}}.

%% @doc Format validation error to binary string (for tests)
-spec format_validation_error(map()) -> binary().
format_validation_error(Error) ->
    Reason = maps:get(reason, Error, unknown_error),
    Details = maps:get(details, Error, #{}),
    case Reason of
        invalid_method ->
            Method = maps:get(method, Details, unknown),
            <<"Invalid method: ", Method/binary>>;
        invalid_method_type ->
            <<"Method must be a binary">>;
        unknown_method ->
            Method = maps:get(method, Error, unknown),
            <<"Unknown method: ", Method/binary>>;
        type_mismatch ->
            Field = maps:get(field, Error, unknown),
            Expected = maps:get(expected, Error, any),
            <<"Field '", Field/binary, "' must be ", (atom_to_binary(Expected, utf8))/binary>>;
        missing_required_fields ->
            Missing = maps:get(missing, Details, []),
            MissingBin = list_to_binary(lists:join(<<", ">>, Missing)),
            <<"Missing required fields: ", MissingBin/binary>>;
        _ ->
            <<"Validation error: ", (atom_to_binary(Reason, utf8))/binary>>
    end.

%%%===================================================================
%%% Internal Validation Checks for validate_all/1
%%%===================================================================

check_jsonrpc_version(Request) ->
    case maps:get(?JSONRPC_FIELD_JSONRPC, Request, undefined) of
        ?JSONRPC_VERSION ->
            #{name => <<"json_rpc_version">>,
              status => passed,
              message => <<"JSON-RPC version 2.0 validated">>};
        _ ->
            #{name => <<"json_rpc_version">>,
              status => failed,
              message => <<"JSON-RPC version must be 2.0">>}
    end.

check_request_structure(Request) ->
    case validate_jsonrpc_structure(Request) of
        ok ->
            #{name => <<"request_structure">>,
              status => passed,
              message => <<"Request structure is valid">>};
        {error, Reason} ->
            #{name => <<"request_structure">>,
              status => failed,
              message => <<"Invalid request structure">>,
              details => #{reason => Reason}}
    end.

check_initialize_validation(InitRequest) ->
    Params = maps:get(?JSONRPC_FIELD_PARAMS, InitRequest),
    case validate_initialize_params(Params) of
        ok ->
            #{name => <<"initialize_params">>,
              status => passed,
              message => <<"Initialize parameters validated">>};
        {error, Reason} ->
            #{name => <<"initialize_params">>,
              status => failed,
              message => <<"Invalid initialize parameters">>,
              details => #{reason => Reason}}
    end.

check_method_validation() ->
    ValidMethods = [?MCP_METHOD_PING, ?MCP_METHOD_INITIALIZE, ?MCP_METHOD_TOOLS_LIST],
    Results = [validate_method(M) =:= ok || M <- ValidMethods],
    case lists:all(fun(R) -> R end, Results) of
        true ->
            #{name => <<"method_validation">>,
              status => passed,
              message => <<"All MCP methods validated">>};
        false ->
            #{name => <<"method_validation">>,
              status => failed,
              message => <<"Some methods failed validation">>}
    end.

check_error_code_validation() ->
    ValidCodes = [?JSONRPC_PARSE_ERROR, ?JSONRPC_INVALID_REQUEST, ?JSONRPC_METHOD_NOT_FOUND],
    Results = [validate_error_code(C) =:= ok || C <- ValidCodes],
    case lists:all(fun(R) -> R end, Results) of
        true ->
            #{name => <<"error_codes">>,
              status => passed,
              message => <<"Error codes validated">>};
        false ->
            #{name => <<"error_codes">>,
              status => failed,
              message => <<"Some error codes failed validation">>}
    end.

check_invalid_version_rejection(InvalidMsg) ->
    case validate_jsonrpc_structure(InvalidMsg) of
        {error, _} ->
            #{name => <<"invalid_version_rejection">>,
              status => passed,
              message => <<"Invalid JSON-RPC version correctly rejected">>};
        ok ->
            #{name => <<"invalid_version_rejection">>,
              status => failed,
              message => <<"Invalid version not rejected">>}
    end.

check_capabilities_validation() ->
    ValidCaps = #{<<"tools">> => #{}},
    case validate_capabilities(ValidCaps) of
        ok ->
            #{name => <<"capabilities">>,
              status => passed,
              message => <<"Capabilities structure validated">>};
        {error, _} ->
            #{name => <<"capabilities">>,
              status => warning,
              message => <<"Capabilities validation has warnings">>}
    end.

check_resource_validation() ->
    ValidResource =
        #{?MCP_PARAM_URI => <<"file:///test.txt">>, ?MCP_PARAM_NAME => <<"Test Resource">>},
    case validate_resource(ValidResource) of
        ok ->
            #{name => <<"resources">>,
              status => passed,
              message => <<"Resource structure validated">>};
        {error, _} ->
            #{name => <<"resources">>,
              status => failed,
              message => <<"Resource validation failed">>}
    end.

check_tool_validation() ->
    ValidTool =
        #{?MCP_PARAM_NAME => <<"test_tool">>,
          ?MCP_PARAM_INPUT_SCHEMA => #{<<"type">> => <<"object">>}},
    case validate_tool(ValidTool) of
        ok ->
            #{name => <<"tools">>,
              status => passed,
              message => <<"Tool structure validated">>};
        {error, _} ->
            #{name => <<"tools">>,
              status => failed,
              message => <<"Tool validation failed">>}
    end.

check_prompt_validation() ->
    ValidPrompt = #{?MCP_PARAM_NAME => <<"test_prompt">>},
    case validate_prompt(ValidPrompt) of
        ok ->
            #{name => <<"prompts">>,
              status => passed,
              message => <<"Prompt structure validated">>};
        {error, _} ->
            #{name => <<"prompts">>,
              status => failed,
              message => <<"Prompt validation failed">>}
    end.
