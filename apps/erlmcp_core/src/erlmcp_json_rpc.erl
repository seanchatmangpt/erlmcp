-module(erlmcp_json_rpc).

-include("erlmcp.hrl").

%% API exports
-export([
    encode_request/3,
    encode_response/2,
    encode_error_response/3,
    encode_error_response/4,
    encode_notification/2,
    decode_message/1,
    decode_message/2,
    decode_batch/1,
    encode_batch/1,
    is_batch_request/1,
    create_error/3,
    create_error_with_data/4,
    validate_error_code/1,
    error_method_not_found/2,
    error_invalid_params/2,
    error_resource_not_found/2,
    error_tool_not_found/2,
    error_prompt_not_found/2,
    error_capability_not_supported/2,
    error_not_initialized/1,
    error_validation_failed/2,
    error_message_too_large/2,
    error_internal/1,
    error_parse/1,
    create_batch_error_response/3
]).

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
    Request = #json_rpc_request{
        id = Id,
        method = Method,
        params = Params
    },
    encode_message(Request).

-spec encode_response(json_rpc_id(), term()) -> binary().
encode_response(Id, Result) ->
    Response = #json_rpc_response{
        id = Id,
        result = Result
    },
    encode_message(Response).

-spec encode_error_response(json_rpc_id(), integer(), binary()) -> binary().
encode_error_response(Id, Code, Message) when is_integer(Code), is_binary(Message) ->
    encode_error_response(Id, Code, Message, undefined).

-spec encode_error_response(json_rpc_id(), integer(), binary(), term()) -> binary().
encode_error_response(Id, Code, Message, Data) when is_integer(Code), is_binary(Message) ->
    %% Validate error code - use internal error if invalid
    FinalCode = case validate_error_code(Code) of
        true -> Code;
        false ->
            logger:warning("Invalid error code ~p, using internal error", [Code]),
            ?JSONRPC_INTERNAL_ERROR
    end,
    Error = build_error_object(FinalCode, Message, Data),
    Response = #json_rpc_response{
        id = Id,
        error = Error
    },
    encode_message(Response).

-spec encode_notification(binary(), json_rpc_params()) -> binary().
encode_notification(Method, Params) when is_binary(Method) ->
    Notification = #json_rpc_notification{
        method = Method,
        params = Params
    },
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
            try jsx:decode(Json, [return_maps]) of
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
    try jsx:decode(Json, [return_maps]) of
        Data when is_list(Data) ->
            parse_batch(Data);
        Data when is_map(Data) ->
            %% Single request, wrap in list
            case erlmcp_message_parser:parse_json_rpc(Data) of
                {ok, Message} -> {ok, [Message]};
                Error -> Error
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
    jsx:encode(Maps).

-spec is_batch_request(binary()) -> boolean().
is_batch_request(Json) when is_binary(Json) ->
    try
        case jsx:decode(Json, [return_maps]) of
            L when is_list(L) -> true;
            _ -> false
        end
    catch
        _:_ -> false
    end.

-spec create_error(integer(), binary(), term()) -> #mcp_error{}.
create_error(Code, Message, Data) when is_integer(Code), is_binary(Message) ->
    #mcp_error{
        code = Code,
        message = Message,
        data = Data
    }.

-spec create_error_with_data(integer(), binary(), atom(), term()) -> #mcp_error{}.
create_error_with_data(Code, Message, DataKey, DataValue)
  when is_integer(Code), is_binary(Message), is_atom(DataKey) ->
    #mcp_error{
        code = Code,
        message = Message,
        data = #{atom_to_binary(DataKey, utf8) => DataValue}
    }.

%%====================================================================
%% Error Code Validation
%%====================================================================

-spec validate_error_code(integer()) -> boolean().
validate_error_code(Code) when is_integer(Code) ->
    lists:member(Code, ?VALID_ERROR_CODES).

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
-spec error_resource_not_found(json_rpc_id(), binary()) -> binary().
error_resource_not_found(Id, Uri) when is_binary(Uri) ->
    Data = #{<<"uri">> => Uri},
    encode_error_response(Id, ?MCP_ERROR_RESOURCE_NOT_FOUND, ?MCP_MSG_RESOURCE_NOT_FOUND, Data).

%% Tool not found error
-spec error_tool_not_found(json_rpc_id(), binary()) -> binary().
error_tool_not_found(Id, ToolName) when is_binary(ToolName) ->
    Data = #{<<"tool">> => ToolName},
    encode_error_response(Id, ?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND, Data).

%% Prompt not found error
-spec error_prompt_not_found(json_rpc_id(), binary()) -> binary().
error_prompt_not_found(Id, PromptName) when is_binary(PromptName) ->
    Data = #{<<"prompt">> => PromptName},
    encode_error_response(Id, ?MCP_ERROR_PROMPT_NOT_FOUND, ?MCP_MSG_PROMPT_NOT_FOUND, Data).

%% Capability not supported error
-spec error_capability_not_supported(json_rpc_id(), binary()) -> binary().
error_capability_not_supported(Id, Capability) when is_binary(Capability) ->
    Data = #{<<"capability">> => Capability},
    encode_error_response(Id, ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, ?MCP_MSG_CAPABILITY_NOT_SUPPORTED, Data).

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
    Data = #{
        <<"maxSize">> => MaxSize,
        <<"unit">> => <<"bytes">>
    },
    encode_error_response(Id, ?MCP_ERROR_MESSAGE_TOO_LARGE, ?MCP_MSG_MESSAGE_TOO_LARGE, Data).

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
        undefined -> null;
        IdVal -> IdVal
    end,
    %% Map error reason to JSON-RPC error codes
    {Code, Message} = map_batch_error_to_code(Reason, Details),
    Error = build_error_object(Code, Message, Details),
    #json_rpc_response{
        id = Id,
        error = Error
    };
create_batch_error_response(_Request, Reason, Details) ->
    %% Completely invalid request (not a map), use null ID
    {Code, Message} = map_batch_error_to_code(Reason, Details),
    Error = build_error_object(Code, Message, Details),
    #json_rpc_response{
        id = null,
        error = Error
    }.

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
binify(Bin) when is_binary(Bin) -> Bin;
binify(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
binify(Int) when is_integer(Int) -> integer_to_binary(Int);
binify(List) when is_list(List) -> list_to_binary(List);
binify(Term) -> term_to_binary(Term).

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
                {ok, Messages} -> {ok, Messages};
                Error -> Error
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
        ok -> validate_batch_version(Rest);
        Error -> Error
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
%% Internal Functions
%%====================================================================

-spec encode_message(json_rpc_message()) -> binary().
encode_message(Message) ->
    Map = build_message_map(Message),
    jsx:encode(Map).

-spec build_message_map(json_rpc_message()) -> map().
build_message_map(#json_rpc_request{id = Id, method = Method, params = Params}) ->
    Base = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => encode_id(Id),
        ?JSONRPC_FIELD_METHOD => Method
    },
    maybe_add_params(Base, Params);

build_message_map(#json_rpc_response{id = Id, result = Result, error = Error}) ->
    Base = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => encode_id(Id)
    },
    add_result_or_error(Base, Result, Error);

build_message_map(#json_rpc_notification{method = Method, params = Params}) ->
    Base = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_METHOD => Method
    },
    maybe_add_params(Base, Params).

-spec encode_id(json_rpc_id()) -> json_rpc_id().
encode_id(null) -> null;
encode_id(Id) when is_binary(Id) -> Id;
encode_id(Id) when is_integer(Id) -> Id.

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
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message
    };
build_error_object(Code, Message, null) ->
    %% Explicit null, don't include data field
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message
    };
build_error_object(Code, Message, Data) when is_map(Data) ->
    %% Valid map data, include it
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
        ?JSONRPC_ERROR_FIELD_DATA => Data
    };
build_error_object(Code, Message, Data) when is_binary(Data) ->
    %% Binary data, wrap in details field
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
        ?JSONRPC_ERROR_FIELD_DATA => #{<<"details">> => Data}
    };
build_error_object(Code, Message, Data) ->
    %% Other data types - convert to binary and wrap in details
    DataBin = erlang:term_to_binary(Data),
    #{
        ?JSONRPC_ERROR_FIELD_CODE => Code,
        ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
        ?JSONRPC_ERROR_FIELD_DATA => #{<<"details">> => DataBin}
    }.

%% Parsing functions moved to erlmcp_message_parser.erl for hot path optimization
%% Import directly from erlmcp_message_parser module
%% Commented out: function is unused, import directly from erlmcp_message_parser
%% parse_json_rpc(Data) ->
%%     erlmcp_message_parser:parse_json_rpc(Data).