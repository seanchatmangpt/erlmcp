%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol Validator for erlmcp - Joe Armstrong's Philosophy
%%%
%%% "ACTUAL VALIDATION OR IT DOESN'T EXIST."
%%%
%%% Not theory. Not "should work". ACTUALLY VALIDATE REAL MESSAGES.
%%%
%%% This module provides comprehensive validation of JSON-RPC 2.0 and
%%% MCP protocol messages against the specification.
%%%
%%% Validation checks:
%%% - JSON-RPC 2.0 structure compliance
%%% - MCP request method names
%%% - MCP notification names
%%% - Error code ranges (-32000 to -32099 for JSON-RPC, 1000-1089 for experimental)
%%% - Field types (binary, integer, map, array)
%%% - Required fields present
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_validator).

-include("erlmcp.hrl").

%% API exports
-export([
    validate_jsonrpc/1,
    validate_request/2,
    validate_response/2,
    validate_notification/2,
    validate_error_code/1,
    validate_against_spec/3,
    validate_method_name/1,
    validate_notification_name/1,
    validate_field_type/3,
    validate_required_fields/2,
    format_validation_error/1
]).

%% Types
-type validation_result() :: ok | {error, validation_error()}.
-type validation_error() :: #{reason := atom(), details := term()}.
-type message_type() :: request | response | notification | error.
-type field_type() :: binary | integer | number | boolean | map | array | any.

-export_type([validation_result/0, validation_error/0, message_type/0, field_type/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate JSON-RPC 2.0 message structure
%% Checks:
%% - jsonrpc field exists and is "2.0"
%% - id field is valid (null, binary, or integer)
%% - method field exists and is binary (for requests/notifications)
%% - Has result OR error, not both (for responses)
-spec validate_jsonrpc(map()) -> validation_result().
validate_jsonrpc(Message) when is_map(Message) ->
    case validate_jsonrpc_version(Message) of
        {error, Reason} -> {error, #{reason => invalid_jsonrpc_version, details => Reason}};
        ok ->
            %% Determine message type and validate accordingly
            case maps:get(<<"id">>, Message, undefined) of
                undefined ->
                    %% Notification - must have method
                    validate_notification_structure(Message);
                _Id ->
                    %% Request or Response
                    case maps:get(<<"method">>, Message, undefined) of
                        undefined -> validate_response_structure(Message);
                        _Method -> validate_request_structure(Message)
                    end
            end
    end;
validate_jsonrpc(_Message) ->
    {error, #{reason => not_map, details => <<"Message must be a map">>}}.

%% @doc Validate MCP request structure
%% Checks:
%% - Method name is valid MCP method
%% - Request params are valid (map or array or undefined)
-spec validate_request(binary(), json_rpc_params()) -> validation_result().
validate_request(Method, Params) when is_binary(Method) ->
    case validate_method_name(Method) of
        ok -> validate_params_structure(Params);
        {error, _} = Error -> Error
    end;
validate_request(Method, _Params) ->
    {error, #{reason => invalid_method_type, details => {method, Method}}}.

%% @doc Validate MCP response structure
%% Checks:
%% - Response ID is valid (null, binary, or integer)
%% - Has exactly one of: result or error
%% - Error object structure is valid (code, message, optional data)
-spec validate_response(json_rpc_id(), term()) -> validation_result().
validate_response(Id, ResultOrError) ->
    case validate_id(Id) of
        ok -> validate_result_or_error(ResultOrError);
        {error, _} = Error -> Error
    end.

%% @doc Validate MCP notification structure
%% Checks:
%% - Notification name is valid MCP notification
%% - Notification params are valid (map or array or undefined)
-spec validate_notification(binary(), json_rpc_params()) -> validation_result().
validate_notification(NotificationName, Params) when is_binary(NotificationName) ->
    case validate_notification_name(NotificationName) of
        ok -> validate_params_structure(Params);
        {error, _} = Error -> Error
    end;
validate_notification(NotificationName, _Params) ->
    {error, #{reason => invalid_notification_type, details => {notification, NotificationName}}}.

%% @doc Validate error code is in valid range
%% Valid ranges:
%% - JSON-RPC 2.0 standard: -32700 to -32600
%% - JSON-RPC server errors: -32099 to -32000
%% - MCP errors: -32113 to -32000
%% - Experimental: 1090-1099
-spec validate_error_code(integer()) -> validation_result().
validate_error_code(Code) when is_integer(Code) ->
    case lists:member(Code, ?VALID_ERROR_CODES) of
        true -> ok;
        false ->
            {error, #{reason => invalid_error_code,
                      details => #{code => Code,
                                   message => <<"Error code not in valid range">>}}}
    end;
validate_error_code(Code) ->
    {error, #{reason => invalid_error_code_type, details => {code, Code}}}.

%% @doc validate against spec (stub for future spec_parser integration)
%% This will be enhanced when spec_parser is available
-spec validate_against_spec(binary(), atom(), term()) -> validation_result().
validate_against_spec(_MethodName, _MessageType, _Payload) ->
    %% TODO: Integrate with erlmcp_spec_parser when available
    %% For now, just validate structure
    ok.

%% @doc Validate MCP method name
%% Checks method name is in the list of valid MCP methods
-spec validate_method_name(binary()) -> validation_result().
validate_method_name(Method) when is_binary(Method) ->
    ValidMethods = get_valid_methods(),
    case lists:member(Method, ValidMethods) of
        true -> ok;
        false ->
            {error, #{reason => unknown_method,
                      details => #{method => Method,
                                   valid_methods => ValidMethods}}}
    end;
validate_method_name(Method) ->
    {error, #{reason => invalid_method_type, details => {method, Method}}}.

%% @doc Validate MCP notification name
%% Checks notification name is in the list of valid MCP notifications
-spec validate_notification_name(binary()) -> validation_result().
validate_notification_name(Notification) when is_binary(Notification) ->
    ValidNotifications = get_valid_notifications(),
    case lists:member(Notification, ValidNotifications) of
        true -> ok;
        false ->
            {error, #{reason => unknown_notification,
                      details => #{notification => Notification,
                                   valid_notifications => ValidNotifications}}}
    end;
validate_notification_name(Notification) ->
    {error, #{reason => invalid_notification_type, details => {notification, Notification}}}.

%% @doc Validate field type
%% Checks field value matches expected type
-spec validate_field_type(binary(), term(), field_type() | [field_type()]) -> validation_result().
validate_field_type(_FieldName, _Value, any) ->
    %% Any type is acceptable
    ok;
validate_field_type(FieldName, Value, ExpectedType) when is_atom(ExpectedType) ->
    validate_field_type(FieldName, Value, [ExpectedType]);
validate_field_type(FieldName, Value, ExpectedTypes) when is_list(ExpectedTypes) ->
    case lists:any(fun(Type) -> type_matches(Value, Type) end, ExpectedTypes) of
        true -> ok;
        false ->
            {error, #{reason => type_mismatch,
                      details => #{field => FieldName,
                                   expected => ExpectedTypes,
                                   actual => get_type(Value)}}}
    end.

%% @doc Validate required fields are present
%% Checks all required fields exist in the message
-spec validate_required_fields(map(), [binary()]) -> validation_result().
validate_required_fields(Message, RequiredFields) when is_map(Message), is_list(RequiredFields) ->
    Missing = lists:filter(fun(Field) ->
        not maps:is_key(Field, Message)
    end, RequiredFields),

    case Missing of
        [] -> ok;
        _ ->
            {error, #{reason => missing_required_fields,
                      details => #{missing => Missing}}}
    end;
validate_required_fields(_Message, _RequiredFields) ->
    {error, #{reason => invalid_message_structure, details => <<"Message must be a map">>}}.

%% @doc Format validation error for logging/response
-spec format_validation_error(validation_error()) -> binary().
format_validation_error(#{reason := Reason, details := Details}) ->
    DetailsBin = format_details(Details),
    ReasonBin = atom_to_binary(Reason, utf8),
    <<ReasonBin/binary, ": ", DetailsBin/binary>>.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Validate jsonrpc version field
validate_jsonrpc_version(#{?JSONRPC_FIELD_JSONRPC := Version}) when is_binary(Version) ->
    case Version of
        ?JSONRPC_VERSION -> ok;
        _ ->
            {error, #{reason => wrong_version,
                      details => #{expected => ?JSONRPC_VERSION,
                                   actual => Version}}}
    end;
validate_jsonrpc_version(_) ->
    {error, #{reason => missing_jsonrpc, details => <<"Missing jsonrpc field">>}}.

%% @private Validate request structure (has id and method)
validate_request_structure(Message) ->
    %% Check id is valid
    Id = maps:get(<<"id">>, Message),
    case validate_id(Id) of
        {error, Reason} -> {error, #{reason => invalid_id, details => Reason}};
        ok ->
            %% Check method exists and is binary
            case maps:get(<<"method">>, Message, undefined) of
                undefined ->
                    {error, #{reason => missing_method, details => <<"Requests must have method field">>}};
                Method when is_binary(Method) ->
                    %% Method validated later against spec
                    ok;
                Method ->
                    {error, #{reason => invalid_method_type, details => #{method => Method}}}
            end
    end.

%% @private Validate response structure (has id and exactly one of: result or error)
validate_response_structure(Message) ->
    %% Check id is valid
    Id = maps:get(<<"id">>, Message),
    case validate_id(Id) of
        {error, Reason} -> {error, #{reason => invalid_id, details => Reason}};
        ok ->
            %% Check exactly one of result or error exists
            HasResult = maps:is_key(<<"result">>, Message),
            HasError = maps:is_key(<<"error">>, Message),

            case {HasResult, HasError} of
                {true, false} -> ok;
                {false, true} ->
                    %% Validate error object structure
                    ErrorObj = maps:get(<<"error">>, Message),
                    validate_error_object(ErrorObj);
                {true, true} ->
                    {error, #{reason => both_result_and_error,
                              details => <<"Response cannot have both result and error">>}};
                {false, false} ->
                    {error, #{reason => missing_result_and_error,
                              details => <<"Response must have exactly one of: result or error">>}}
            end
    end.

%% @private Validate notification structure (no id, has method)
validate_notification_structure(Message) ->
    %% Check method exists and is binary
    case maps:get(<<"method">>, Message, undefined) of
        undefined ->
            {error, #{reason => missing_method, details => <<"Notifications must have method field">>}};
        Method when is_binary(Method) ->
            %% Notification validated later against spec
            ok;
        Method ->
            {error, #{reason => invalid_method_type, details => #{method => Method}}}
    end.

%% @private Validate request ID (null, binary, or integer)
validate_id(null) -> ok;
validate_id(Id) when is_binary(Id) ->
    case byte_size(Id) > 0 of
        true -> ok;
        false -> {error, #{reason => empty_id, details => <<"ID cannot be empty binary">>}}
    end;
validate_id(Id) when is_integer(Id) ->
    case Id >= 0 of
        true -> ok;
        false -> {error, #{reason => negative_id, details => #{id => Id}}}
    end;
validate_id(Id) ->
    {error, #{reason => invalid_id_type, details => #{id => Id, type => get_type(Id)}}}.

%% @private Validate error object structure
validate_error_object(ErrorObj) when is_map(ErrorObj) ->
    %% Check required fields: code, message
    Code = maps:get(<<"code">>, ErrorObj, undefined),
    Message = maps:get(<<"message">>, ErrorObj, undefined),

    case {Code, Message} of
        {undefined, _} ->
            {error, #{reason => missing_error_code, details => <<"Error object must have code field">>}};
        {_, undefined} ->
            {error, #{reason => missing_error_message, details => <<"Error object must have message field">>}};
        {CodeVal, MessageVal} when is_integer(CodeVal), is_binary(MessageVal) ->
            %% Validate error code is in valid range
            case validate_error_code(CodeVal) of
                ok -> ok;
                {error, _} = Error -> Error
            end;
        {CodeVal, _MessageVal} when not is_integer(CodeVal) ->
            {error, #{reason => invalid_error_code_type, details => #{code => CodeVal}}};
        {_CodeVal, MessageVal} ->
            {error, #{reason => invalid_error_message_type, details => #{message => MessageVal}}}
    end;
validate_error_object(ErrorObj) ->
    {error, #{reason => invalid_error_type, details => #{error => ErrorObj}}}.

%% @private Validate result or error field
validate_result_or_error(ResultOrError) when is_map(ResultOrError) ->
    %% Check if it's an error object (has code and message fields)
    case {maps:get(<<"code">>, ResultOrError, undefined), maps:get(<<"message">>, ResultOrError, undefined)} of
        {Code, Message} when is_integer(Code), is_binary(Message) ->
            %% This is an error object, validate it
            validate_error_object(ResultOrError);
        _ ->
            %% This is a regular result map, accept it
            ok
    end;
validate_result_or_error(_ResultOrError) ->
    %% Result can be any type
    ok.

%% @private Validate params structure (map, array, or undefined)
validate_params_structure(undefined) -> ok;
validate_params_structure(Params) when is_map(Params) -> ok;
validate_params_structure(Params) when is_list(Params) -> ok;
validate_params_structure(Params) ->
    {error, #{reason => invalid_params_type,
              details => #{params => Params, expected => <<"map, array, or undefined">>}}}.

%% @private Check if value matches type
type_matches(_Value, any) -> true;
type_matches(Value, binary) when is_binary(Value) -> true;
type_matches(Value, integer) when is_integer(Value) -> true;
type_matches(Value, number) when is_number(Value) -> true;
type_matches(Value, boolean) when is_boolean(Value) -> true;
type_matches(Value, map) when is_map(Value) -> true;
type_matches(Value, array) when is_list(Value) -> true;
type_matches(_Value, _Type) -> false.

%% @private Get type of value as atom
get_type(Value) when is_binary(Value) -> binary;
get_type(Value) when is_integer(Value) -> integer;
get_type(Value) when is_float(Value) -> number;
get_type(Value) when is_boolean(Value) -> boolean;
get_type(Value) when is_map(Value) -> map;
get_type(Value) when is_list(Value) -> array;
get_type(_Value) -> unknown.

%% @private Format error details
format_details(Details) when is_map(Details) ->
    %% Convert map details to binary
    maps:fold(fun(Key, Value, Acc) ->
        KeyBin = to_binary(Key),
        ValueBin = to_binary(Value),
        <<Acc/binary, KeyBin/binary, "=", ValueBin/binary, " ">>
    end, <<"">>, Details);
format_details(Details) when is_binary(Details) ->
    Details;
format_details(Details) ->
    iolist_to_binary(io_lib:format("~p", [Details])).

%% @private Convert term to binary
to_binary(Term) when is_binary(Term) -> Term;
to_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) -> integer_to_binary(Term);
to_binary(Term) -> iolist_to_binary(io_lib:format("~p", [Term])).

%% @private Get list of valid MCP methods
get_valid_methods() ->
    [
        %% Core Protocol
        ?MCP_METHOD_INITIALIZE,
        ?MCP_METHOD_PING,

        %% Resources
        ?MCP_METHOD_RESOURCES_LIST,
        ?MCP_METHOD_RESOURCES_READ,
        ?MCP_METHOD_RESOURCES_TEMPLATES_LIST,
        ?MCP_METHOD_RESOURCES_SUBSCRIBE,
        ?MCP_METHOD_RESOURCES_UNSUBSCRIBE,

        %% Tools
        ?MCP_METHOD_TOOLS_LIST,
        ?MCP_METHOD_TOOLS_CALL,

        %% Tasks (NEW in MCP 2025-11-25)
        ?MCP_METHOD_TASKS_CREATE,
        ?MCP_METHOD_TASKS_LIST,
        ?MCP_METHOD_TASKS_GET,
        ?MCP_METHOD_TASKS_RESULT,
        ?MCP_METHOD_TASKS_CANCEL,

        %% Prompts
        ?MCP_METHOD_PROMPTS_LIST,
        ?MCP_METHOD_PROMPTS_GET,

        %% Completion (MCP 2025-11-25)
        ?MCP_METHOD_COMPLETION_COMPLETE,

        %% Elicitation (MCP 2025-11-25)
        ?MCP_METHOD_ELICITATION_CREATE,

        %% Cancellation
        ?MCP_METHOD_REQUESTS_CANCEL,

        %% Logging
        ?MCP_METHOD_LOGGING_SET_LEVEL,

        %% Roots
        ?MCP_METHOD_ROOTS_LIST,

        %% Sampling
        ?MCP_METHOD_SAMPLING_CREATE_MESSAGE
    ].

%% @private Get list of valid MCP notifications
get_valid_notifications() ->
    [
        %% Core Protocol
        ?MCP_METHOD_INITIALIZED,

        %% Progress
        ?MCP_METHOD_NOTIFICATIONS_PROGRESS,

        %% Resources
        ?MCP_METHOD_NOTIFICATIONS_RESOURCES_UPDATED,
        ?MCP_METHOD_NOTIFICATIONS_RESOURCES_LIST_CHANGED,

        %% Tools
        ?MCP_METHOD_NOTIFICATIONS_TOOLS_LIST_CHANGED,

        %% Prompts
        ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,

        %% Roots
        ?MCP_METHOD_NOTIFICATIONS_ROOTS_LIST_CHANGED,

        %% Tasks (NEW in MCP 2025-11-25)
        ?MCP_METHOD_NOTIFICATIONS_TASKS_STATUS,

        %% Cancellation
        ?MCP_METHOD_NOTIFICATIONS_CANCELLED,

        %% Elicitation (MCP 2025-11-25)
        ?MCP_METHOD_NOTIFICATIONS_ELICITATION_COMPLETE,

        %% Message (MCP 2025-11-25)
        ?MCP_METHOD_NOTIFICATIONS_MESSAGE
    ].
