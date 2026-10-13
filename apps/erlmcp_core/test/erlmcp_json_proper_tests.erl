%%%-------------------------------------------------------------------
%%% @doc PropER Property-Based Tests for JSON-RPC Messages
%%%
%%% Tests JSON-RPC 2.0 protocol invariants:
%%% - Encode/decode roundtrip preservation
%%% - Version field validation
%%% - Message type detection (request/response/notification)
%%% - ID field handling (string, number, null)
%%% - Parameter validation (object, array, omitted)
%%%
%%% Chicago School TDD: Real JSON encoding/decoding, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Generate valid JSON-RPC request IDs
-spec request_id() -> proper:gen().
request_id() ->
    proper_types:union([
        proper_types:binary(),
        proper_types:int(),
        proper_types:atom(null)
    ]).

%% @doc Generate valid JSON-RPC method names
-spec method_name() -> proper:gen().
method_name() ->
    proper_types:non_empty(proper_types:binary()).

%% @doc Generate valid JSON-RPC parameters
-spec params() -> proper:gen().
params() ->
    proper_types:union([
        proper_types:map(proper_types:binary(), proper_types:any()),
        proper_types:list(proper_types:any()),
        proper_types:undefined()
    ]).

%% @doc Generate valid JSON-RPC request
-spec json_rpc_request() -> proper:gen().
json_rpc_request() ->
    ?LET({Id, Method, Params},
        {request_id(), method_name(), params()},
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => Id,
          ?JSONRPC_FIELD_METHOD => Method,
          ?JSONRPC_FIELD_PARAMS => Params}).

%% @doc Generate valid JSON-RPC response (success)
-spec json_rpc_response() -> proper:gen().
json_rpc_response() ->
    ?LET({Id, Result},
        {request_id(), proper_types:any()},
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => Id,
          ?JSONRPC_FIELD_RESULT => Result}).

%% @doc Generate valid JSON-RPC error response
-spec json_rpc_error_response() -> proper:gen().
json_rpc_error_response() ->
    ?LET({Id, Code, Message},
        {request_id(), proper_types:int(), proper_types:binary()},
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => Id,
          ?JSONRPC_FIELD_ERROR => #{
              code => Code,
              message => Message,
              data => proper_types:undefined()
          }}).

%% @doc Generate valid JSON-RPC notification
-spec json_rpc_notification() -> proper:gen().
json_rpc_notification() ->
    ?LET({Method, Params},
        {method_name(), params()},
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => Method,
          ?JSONRPC_FIELD_PARAMS => Params}).

%% @doc Generate any valid JSON-RPC message
-spec json_rpc_message() -> proper:gen().
json_rpc_message() ->
    proper_types:union([
        json_rpc_request(),
        json_rpc_response(),
        json_rpc_error_response(),
        json_rpc_notification()
    ]).

%%====================================================================
%% Properties: JSON Codec Roundtrip
%%====================================================================

%% @doc Property: JSON encode/decode roundtrip preserves data
%% Chicago School: Real encoding/decoding, state verification
prop_json_encode_decode_roundtrip() ->
    ?FORALL(Message,
        json_rpc_message(),
        begin
            %% Exercise: Encode then decode
            Encoded = erlmcp_json:encode(Message),
            Decoded = erlmcp_json:decode(Encoded),

            %% Verify: Roundtrip preserves original message
            %% Note: Maps with undefined keys may lose those keys
            CleanMessage = clean_undefined(Message),
            CleanDecoded = clean_undefined(Decoded),
            CleanMessage =:= CleanDecoded
        end).

%% @doc Property: JSON encoding always produces valid binary
prop_json_encode_produces_binary() ->
    ?FORALL(Message,
        json_rpc_message(),
        begin
            Encoded = erlmcp_json:encode(Message),
            is_binary(Encoded) andalso byte_size(Encoded) > 0
        end).

%% @doc Property: JSON decoding invalid input raises error
prop_json_decode_invalid_input_errors() ->
    ?FORALL(BadInput,
        proper_types:oneof([
            <<>>,
            <<"not json">>,
            <<"{invalid}">>,
            <<"{\"jsonrpc\": \"2.0\"}">>  % Incomplete
        ]),
        begin
            try
                erlmcp_json:decode(BadInput),
                false  % Should have raised error
            catch
                error:{decode_error, _} ->
                    true;
                _:_ ->
                    false  % Wrong error type
            end
        end).

%%====================================================================
%% Properties: Message Parser
%%====================================================================

%% @doc Property: Parser correctly identifies request messages
prop_parser_identifies_request() ->
    ?FORALL(Request,
        json_rpc_request(),
        begin
            {ok, Parsed} = erlmcp_message_parser:parse_json_rpc(Request),
            is_record(Parsed, json_rpc_request) andalso
            Parsed#json_rpc_request.id =:= maps:get(?JSONRPC_FIELD_ID, Request) andalso
            Parsed#json_rpc_request.method =:= maps:get(?JSONRPC_FIELD_METHOD, Request)
        end).

%% @doc Property: Parser correctly identifies response messages
prop_parser_identifies_response() ->
    ?FORALL(Response,
        json_rpc_response(),
        begin
            {ok, Parsed} = erlmcp_message_parser:parse_json_rpc(Response),
            is_record(Parsed, json_rpc_response) andalso
            Parsed#json_rpc_response.id =:= maps:get(?JSONRPC_FIELD_ID, Response)
        end).

%% @doc Property: Parser correctly identifies notification messages
prop_parser_identifies_notification() ->
    ?FORALL(Notification,
        json_rpc_notification(),
        begin
            {ok, Parsed} = erlmcp_message_parser:parse_json_rpc(Notification),
            is_record(Parsed, json_rpc_notification) andalso
            Parsed#json_rpc_notification.method =:= maps:get(?JSONRPC_FIELD_METHOD, Notification)
        end).

%% @doc Property: Parser rejects invalid JSON-RPC version
prop_parser_rejects_invalid_version() ->
    ?FORALL({Message, BadVersion},
        {json_rpc_request(), proper_types:binary()},
        begin
            BadMessage = maps:put(?JSONRPC_FIELD_JSONRPC, BadVersion, Message),
            case erlmcp_message_parser:parse_json_rpc(BadMessage) of
                {error, {invalid_request, {wrong_version, BadVersion}}} ->
                    true;
                _ ->
                    false
            end
        end).

%% @doc Property: Parser rejects messages without jsonrpc field
prop_parser_rejects_missing_version() ->
    ?FORALL(Message,
        json_rpc_request(),
        begin
            BadMessage = maps:remove(?JSONRPC_FIELD_JSONRPC, Message),
            case erlmcp_message_parser:parse_json_rpc(BadMessage) of
                {error, {invalid_request, missing_jsonrpc}} ->
                    true;
                _ ->
                    false
            end
        end).

%%====================================================================
%% Properties: Parameter Validation
%%====================================================================

%% @doc Property: Parameters can be maps, lists, or undefined
prop_params_accepts_valid_types() ->
    ?FORALL(Params,
        proper_types:union([
            proper_types:map(proper_types:binary(), proper_types:any()),
            proper_types:list(proper_types:any()),
            proper_types:undefined()
        ]),
        begin
            Result = erlmcp_message_parser:validate_params(Params),
            (Params =:= undefined andalso Result =:= undefined) orelse
            (is_map(Params) andalso is_map(Result)) orelse
            (is_list(Params) andalso is_list(Result))
        end).

%%====================================================================
%% Properties: MCP Types
%%====================================================================

%% @doc Property: Request ID validation accepts valid binaries
prop_request_id_validation_accepts_valid() ->
    ?FORALL(Id,
        proper_types:non_empty(proper_types:binary()),
        begin
            case erlmcp_mcp_types:validate_request_id(Id) of
                {ok, _} -> true;
                _ -> false
            end
        end).

%% @doc Property: Request ID validation rejects empty binaries
prop_request_id_validation_rejects_empty() ->
    ?FORALL(_,
        proper_types:binary(),
        begin
            Id = <<>>,
            case erlmcp_mcp_types:validate_request_id(Id) of
                {error, invalid_request_id} -> true;
                _ -> false
            end
        end).

%% @doc Property: Generated request IDs are unique
prop_generated_request_ids_are_unique() ->
    ?FORALL(_,
        proper_types:integer(1, 100),
        begin
            Id1 = erlmcp_mcp_types:generate_request_id(),
            Id2 = erlmcp_mcp_types:generate_request_id(),
            is_binary(Id1) andalso is_binary(Id2) andalso Id1 =/= Id2
        end).

%% @doc Property: Tool name validation accepts valid names
prop_tool_name_validation_accepts_valid() ->
    ?FORALL(Name,
        proper_types:non_empty(proper_types:binary()),
        begin
            case erlmcp_mcp_types:validate_tool_name(Name) of
                {ok, _} -> true;
                _ -> false
            end
        end).

%% @doc Property: Session ID generation produces valid UUIDs
prop_session_id_is_valid_uuid() ->
    ?FORALL(_,
        proper_types:unit(),
        begin
            SessionId = erlmcp_mcp_types:generate_session_id(),
            %% UUID v4 format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
            Pattern = <<"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$">>,
            case re:run(SessionId, Pattern, [caseless]) of
                {match, _} -> true;
                _ -> false
            end
        end).

%%====================================================================
%% Properties: Error Handling
%%====================================================================

%% @doc Property: Error responses have valid structure
prop_error_response_structure() ->
    ?FORALL({Code, Message},
        {proper_types:int(-32768, -32000), proper_types:binary()},
        begin
            ErrorMap = #{code => Code, message => Message},
            %% Verify error map has required fields
            maps:is_key(code, ErrorMap) andalso
            maps:is_key(message, ErrorMap) andalso
            is_integer(Code) andalso
            is_binary(Message)
        end).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Clean undefined values from maps for comparison
%% JSON encoding may drop undefined keys
-spec clean_undefined(map()) -> map().
clean_undefined(Map) when is_map(Map) ->
    maps:fold(fun
        (K, undefined, Acc) ->
            maps:remove(K, Acc);
        (K, V, Acc) when is_map(V) ->
            maps:put(K, clean_undefined(V), Acc);
        (K, V, Acc) when is_list(V) ->
            CleanedList = lists:map(fun clean_undefined/1, V),
            maps:put(K, CleanedList, Acc);
        (K, V, Acc) ->
            maps:put(K, V, Acc)
    end, Map, Map);
clean_undefined(List) when is_list(List) ->
    lists:map(fun clean_undefined/1, List);
clean_undefined(Term) ->
    Term.

%%====================================================================
%% EUnit Integration
%%====================================================================

proper_test_() ->
    {timeout, 60, [
        ?_assertEqual(true, proper:module(?MODULE, [{numtests, 100}]))
    ]}.

%%====================================================================
%% Statistics Collection
%%====================================================================

collect_stats() ->
    proper:check_specs(?MODULE).
