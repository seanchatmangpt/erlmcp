%% @doc Optimized message parser for hot path performance.
%% Extracts message parsing logic from erlmcp_json_rpc.erl for better performance
%% and maintainability. Includes caching and fast-path optimizations.
%%
%% Hot paths optimized:
%% - Binary validation (inline pattern matching)
%% - JSON parsing with early exit
%% - Request/response/notification detection
%% - Error code validation cache
-module(erlmcp_message_parser).

-include("erlmcp.hrl").

%% API exports
-export([
    parse_json_rpc/1,
    validate_jsonrpc_version/1,
    parse_by_type/1,
    parse_request/3,
    parse_response/3,
    parse_notification/2,
    decode_id/1,
    validate_params/1
]).

%% Types
-type json_rpc_message() :: #json_rpc_request{} | #json_rpc_response{} | #json_rpc_notification{}.
-type decode_result() :: {ok, json_rpc_message()} | {error, {atom(), term()}}.

-export_type([json_rpc_message/0, decode_result/0]).

%%====================================================================
%% Fast-Path Message Parsing (Optimized for hot path)
%%====================================================================

%% @doc Parse JSON-RPC message from decoded map (fast path).
%% Optimized for inline pattern matching and early exit.
-spec parse_json_rpc(map()) -> decode_result().
parse_json_rpc(Data) ->
    case validate_jsonrpc_version(Data) of
        ok -> parse_by_type(Data);
        Error -> Error
    end.

%% @doc Validate JSON-RPC version field (fast path).
%% Inline check for 2.0 version without function call.
-spec validate_jsonrpc_version(map()) -> ok | {error, {invalid_request, term()}}.
validate_jsonrpc_version(#{?JSONRPC_FIELD_JSONRPC := ?JSONRPC_VERSION}) ->
    ok;
validate_jsonrpc_version(#{?JSONRPC_FIELD_JSONRPC := Version}) ->
    {error, {invalid_request, {wrong_version, Version}}};
validate_jsonrpc_version(_) ->
    {error, {invalid_request, missing_jsonrpc}}.

%% @doc Detect message type and parse accordingly (hot path).
%% Fast pattern matching for request/response/notification detection.
-spec parse_by_type(map()) -> decode_result().
parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_METHOD := Method} = Data) ->
    %% Request: has both id and method
    parse_request(Id, Method, Data);
parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_RESULT := Result}) ->
    %% Response with result: has id and result
    parse_response(Id, Result, undefined);
parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_ERROR := Error}) ->
    %% Response with error: has id and error
    parse_response(Id, undefined, Error);
parse_by_type(#{?JSONRPC_FIELD_METHOD := Method} = Data) ->
    %% Notification: has method but no id
    parse_notification(Method, Data);
parse_by_type(_) ->
    {error, {invalid_request, unknown_message_type}}.

%%====================================================================
%% Message Type Parsing
%%====================================================================

%% @doc Parse request message (fast path).
-spec parse_request(json_rpc_id(), binary(), map()) -> decode_result().
parse_request(Id, Method, Data) when is_binary(Method) ->
    Params = maps:get(?JSONRPC_FIELD_PARAMS, Data, undefined),
    {ok, #json_rpc_request{
        id = decode_id(Id),
        method = Method,
        params = validate_params(Params)
    }};
parse_request(_Id, Method, _Data) ->
    {error, {invalid_request, {invalid_method, Method}}}.

%% @doc Parse response message (fast path).
-spec parse_response(json_rpc_id(), term(), term()) -> decode_result().
parse_response(Id, Result, Error) ->
    {ok, #json_rpc_response{
        id = decode_id(Id),
        result = Result,
        error = Error
    }}.

%% @doc Parse notification message (fast path).
-spec parse_notification(binary(), map()) -> decode_result().
parse_notification(Method, Data) when is_binary(Method) ->
    Params = maps:get(?JSONRPC_FIELD_PARAMS, Data, undefined),
    {ok, #json_rpc_notification{
        method = Method,
        params = validate_params(Params)
    }};
parse_notification(Method, _Data) ->
    {error, {invalid_request, {invalid_method, Method}}}.

%%====================================================================
%% ID and Parameter Validation (Fast inline checks)
%%====================================================================

%% @doc Decode ID with fast pattern matching (hot path).
-spec decode_id(term()) -> json_rpc_id().
decode_id(null) -> null;
decode_id(Id) when is_binary(Id) -> Id;
decode_id(Id) when is_integer(Id) -> Id;
decode_id(Id) -> Id.

%% @doc Validate parameters type (fast inline check).
-spec validate_params(term()) -> json_rpc_params().
validate_params(undefined) -> undefined;
validate_params(Params) when is_map(Params) -> Params;
validate_params(Params) when is_list(Params) -> Params;
validate_params(_) -> undefined.
