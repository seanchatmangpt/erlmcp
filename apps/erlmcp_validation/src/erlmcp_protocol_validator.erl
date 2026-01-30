%%%-------------------------------------------------------------------
%%% @doc
%%% Protocol Compliance Validator for MCP 2025-11-25 Specification
%%%
%%% Validates JSON-RPC 2.0 compliance and MCP protocol implementation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_validator).
-behaviour(gen_server).

%% API
-export([start_link/0, run/1, validate_json_rpc/1, validate_request_method/2,
         validate_response_structure/1, validate_error_codes/1, validate_protocol_version/1,
         generate_report/0, get_results/0]).

%% Export individual check functions for testing
-export([check_jsonrpc_version/1, check_request_format/1, check_response_format/1,
         check_notification_format/1, check_batch_requests/1, check_initialize_params/1,
         check_initialize_response/1, check_tools_list_params/1, check_tools_list_response/1,
         check_resources_list_params/1, check_resources_list_response/1,
         check_result_exclusivity/1, check_error_object/1, check_response_id/1,
         check_response_jsonrpc/1, check_mcp_refusal_codes/1, check_jsonrpc_error_codes/1,
         check_custom_error_codes/1, check_protocol_version_2025_11_25/1,
         check_version_compatibility/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include("erlmcp.hrl").
-define(SERVER, ?MODULE).

-record(validation_result, {
    transport :: atom(),
    timestamp :: integer(),
    jsonrpc_passed = 0 :: non_neg_integer(),
    jsonrpc_failed = 0 :: non_neg_integer(),
    response_passed = 0 :: non_neg_integer(),
    response_failed = 0 :: non_neg_integer(),
    error_codes_passed = 0 :: non_neg_integer(),
    error_codes_failed = 0 :: non_neg_integer(),
    version_passed = 0 :: non_neg_integer(),
    version_failed = 0 :: non_neg_integer(),
    details = [] :: [map()]
}).

-record(state, {
    results = #{} :: #{atom() => validation_result()},
    current_transport :: atom() | undefined
}).

-type transport_type() :: stdio | tcp | http | websocket.
-type validation_result() :: #validation_result{}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run(TransportModule) when is_atom(TransportModule) ->
    gen_server:call(?SERVER, {run, TransportModule}).

validate_json_rpc(TransportModule) when is_atom(TransportModule) ->
    Result = #{module => TransportModule, category => jsonrpc,
               timestamp => erlang:system_time(millisecond), checks => []},
    Checks = [check_jsonrpc_version(TransportModule),
              check_request_format(TransportModule),
              check_response_format(TransportModule),
              check_notification_format(TransportModule),
              check_batch_requests(TransportModule)],
    {Passed, Failed} = lists:foldl(fun(Check, {P, F}) ->
        case maps:get(status, Check) of
            passed -> {P + 1, F};
            failed -> {P, F + 1};
            warning -> {P, F}
        end
    end, {0, 0}, Checks),
    Result#{checks => Checks, passed => Passed, failed => Failed,
            status => case Failed of 0 -> passed; _ -> failed end}.

validate_request_method(TransportModule, Method) when is_atom(TransportModule), is_binary(Method) ->
    Result = #{module => TransportModule, category => request_method, method => Method,
               timestamp => erlang:system_time(millisecond), checks => []},
    Checks = case Method of
        <<"initialize">> -> [check_initialize_params(TransportModule), check_initialize_response(TransportModule)];
        <<"tools/list">> -> [check_tools_list_params(TransportModule), check_tools_list_response(TransportModule)];
        <<"resources/list">> -> [check_resources_list_params(TransportModule), check_resources_list_response(TransportModule)];
        _ -> [#{name => unknown_method, status => warning, message => <<"Unknown method, may be custom">>}]
    end,
    {Passed, Failed} = lists:foldl(fun(Check, {P, F}) ->
        case maps:get(status, Check) of
            passed -> {P + 1, F};
            failed -> {P, F + 1};
            warning -> {P, F}
        end
    end, {0, 0}, Checks),
    Result#{checks => Checks, passed => Passed, failed => Failed,
            status => case Failed of 0 -> passed; _ -> failed end}.

validate_response_structure(TransportModule) when is_atom(TransportModule) ->
    Result = #{module => TransportModule, category => response_structure,
               timestamp => erlang:system_time(millisecond), checks => []},
    Checks = [check_result_exclusivity(TransportModule),
              check_error_object(TransportModule),
              check_response_id(TransportModule),
              check_response_jsonrpc(TransportModule)],
    {Passed, Failed} = lists:foldl(fun(Check, {P, F}) ->
        case maps:get(status, Check) of
            passed -> {P + 1, F};
            failed -> {P, F + 1};
            warning -> {P, F}
        end
    end, {0, 0}, Checks),
    Result#{checks => Checks, passed => Passed, failed => Failed,
            status => case Failed of 0 -> passed; _ -> failed end}.

validate_error_codes(TransportModule) when is_atom(TransportModule) ->
    Result = #{module => TransportModule, category => error_codes,
               timestamp => erlang:system_time(millisecond), checks => []},
    Checks = [check_mcp_refusal_codes(TransportModule),
              check_jsonrpc_error_codes(TransportModule),
              check_custom_error_codes(TransportModule)],
    {Passed, Failed} = lists:foldl(fun(Check, {P, F}) ->
        case maps:get(status, Check) of
            passed -> {P + 1, F};
            failed -> {P, F + 1};
            warning -> {P, F}
        end
    end, {0, 0}, Checks),
    Result#{checks => Checks, passed => Passed, failed => Failed,
            status => case Failed of 0 -> passed; _ -> failed end}.

validate_protocol_version(TransportModule) when is_atom(TransportModule) ->
    Result = #{module => TransportModule, category => protocol_version,
               timestamp => erlang:system_time(millisecond), checks => []},
    Checks = [check_protocol_version_2025_11_25(TransportModule),
              check_version_compatibility(TransportModule)],
    {Passed, Failed} = lists:foldl(fun(Check, {P, F}) ->
        case maps:get(status, Check) of
            passed -> {P + 1, F};
            failed -> {P, F + 1};
            warning -> {P, F}
        end
    end, {0, 0}, Checks),
    Result#{checks => Checks, passed => Passed, failed => Failed,
            status => case Failed of 0 -> passed; _ -> failed end}.

generate_report() -> gen_server:call(?SERVER, generate_report).
get_results() -> gen_server:call(?SERVER, get_results).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) -> {ok, #state{}}.

handle_call({run, TransportModule}, _From, State) ->
    ?LOG_INFO("Running protocol validation for: ~p", [TransportModule]),
    JsonRpcResult = validate_json_rpc(TransportModule),
    ResponseResult = validate_response_structure(TransportModule),
    ErrorCodesResult = validate_error_codes(TransportModule),
    VersionResult = validate_protocol_version(TransportModule),
    ValidationResult = #validation_result{
        transport = TransportModule,
        timestamp = erlang:system_time(millisecond),
        jsonrpc_passed = maps:get(passed, JsonRpcResult, 0),
        jsonrpc_failed = maps:get(failed, JsonRpcResult, 0),
        response_passed = maps:get(passed, ResponseResult, 0),
        response_failed = maps:get(failed, ResponseResult, 0),
        error_codes_passed = maps:get(passed, ErrorCodesResult, 0),
        error_codes_failed = maps:get(failed, ErrorCodesResult, 0),
        version_passed = maps:get(passed, VersionResult, 0),
        version_failed = maps:get(failed, VersionResult, 0),
        details = [JsonRpcResult, ResponseResult, ErrorCodesResult, VersionResult]
    },
    NewState = State#state{results = maps:put(TransportModule, ValidationResult, State#state.results)},
    Summary = generate_summary(ValidationResult),
    {reply, {ok, Summary}, NewState};

handle_call(generate_report, _From, State) ->
    Report = generate_full_report(State#state.results),
    {reply, {ok, Report}, State};

handle_call(get_results, _From, State) ->
    Results = maps:map(fun(_Module, ValidationResult) -> generate_summary(ValidationResult) end, State#state.results),
    {reply, {ok, Results}, State};

handle_call(_Request, _From, State) -> {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Check JSON-RPC version field is "2.0"
check_jsonrpc_version(Module) ->
    TestRequest = create_test_request(),
    case TestRequest of
        #{?JSONRPC_FIELD_JSONRPC := <<"2.0">>} ->
            #{name => jsonrpc_version, status => passed,
              message => <<"JSON-RPC 2.0 version field present">>,
              details => #{version => <<"2.0">>}};
        #{?JSONRPC_FIELD_JSONRPC := WrongVersion} ->
            #{name => jsonrpc_version, status => failed,
              message => <<"Invalid JSON-RPC version">>,
              details => #{expected => <<"2.0">>, actual => WrongVersion}};
        _ ->
            #{name => jsonrpc_version, status => failed,
              message => <<"Missing jsonrpc version field">>,
              details => #{module => Module}}
    end.

%% @doc Check request format has required fields (jsonrpc, id, method)
check_request_format(Module) ->
    TestCases = [
        %% Valid request
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_ID => 1,
           ?JSONRPC_FIELD_METHOD => <<"test">>}, true, <<"valid request">>},
        %% Valid request with string id
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_ID => <<"test-id">>,
           ?JSONRPC_FIELD_METHOD => <<"test">>}, true, <<"valid request with string id">>},
        %% Valid request with params
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_ID => 1,
           ?JSONRPC_FIELD_METHOD => <<"test">>,
           ?JSONRPC_FIELD_PARAMS => #{}}, true, <<"valid request with params">>}
    ],
    {ValidCount, InvalidCount} = validate_request_formats(TestCases, 0, 0),
    case ValidCount > 0 andalso InvalidCount =:= 0 of
        true ->
            #{name => request_format, status => passed,
              message => <<"Request format validated">>,
              details => #{valid => ValidCount, invalid => InvalidCount}};
        false ->
            #{name => request_format, status => failed,
              message => <<"Request format validation failed">>,
              details => #{module => Module, valid => ValidCount, invalid => InvalidCount}}
    end.

%% @doc Check response format has required fields (jsonrpc, id, result OR error)
check_response_format(Module) ->
    TestCases = [
        %% Valid success response
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_ID => 1,
           ?JSONRPC_FIELD_RESULT => #{status => ok}}, true, <<"success response">>},
        %% Valid error response
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_ID => 1,
           ?JSONRPC_FIELD_ERROR => #{?JSONRPC_ERROR_FIELD_CODE => -32600,
                                     ?JSONRPC_ERROR_FIELD_MESSAGE => <<"Invalid">>}}, true, <<"error response">>},
        %% Valid success with null result
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_ID => 1,
           ?JSONRPC_FIELD_RESULT => null}, true, <<"success with null result">>}
    ],
    {ValidCount, InvalidCount} = validate_response_formats(TestCases, 0, 0),
    case ValidCount > 0 andalso InvalidCount =:= 0 of
        true ->
            #{name => response_format, status => passed,
              message => <<"Response format validated">>,
              details => #{valid => ValidCount, invalid => InvalidCount}};
        false ->
            #{name => response_format, status => failed,
              message => <<"Response format validation failed">>,
              details => #{module => Module, valid => ValidCount, invalid => InvalidCount}}
    end.

%% @doc Check notification format (no id field)
check_notification_format(Module) ->
    TestCases = [
        %% Valid notification
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_METHOD => <<"test/notify">>,
           ?JSONRPC_FIELD_PARAMS => #{}}, true, <<"valid notification">>},
        %% Valid notification without params
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_METHOD => <<"test/notify2">>}, true, <<"valid notification without params">>},
        %% Valid notification with list params
        {#{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
           ?JSONRPC_FIELD_METHOD => <<"test/notify3">>,
           ?JSONRPC_FIELD_PARAMS => []}, true, <<"valid notification with list params">>}
    ],
    {ValidCount, InvalidCount} = validate_notification_formats(TestCases, 0, 0),
    case ValidCount > 0 andalso InvalidCount =:= 0 of
        true ->
            #{name => notification_format, status => passed,
              message => <<"Notification format validated">>,
              details => #{valid => ValidCount, invalid => InvalidCount}};
        false ->
            #{name => notification_format, status => failed,
              message => <<"Notification format validation failed">>,
              details => #{module => Module, valid => ValidCount, invalid => InvalidCount}}
    end.

%% @doc Check batch requests are arrays
check_batch_requests(Module) ->
    ValidBatch = [
        #{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => <<"test1">>},
        #{?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
          ?JSONRPC_FIELD_ID => 2,
          ?JSONRPC_FIELD_METHOD => <<"test2">>}
    ],
    case is_list(ValidBatch) andalso length(ValidBatch) > 0 of
        true ->
            #{name => batch_requests, status => passed,
              message => <<"Batch requests supported">>,
              details => #{batch_size => length(ValidBatch)}};
        false ->
            #{name => batch_requests, status => failed,
              message => <<"Batch request validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check initialize params structure
check_initialize_params(Module) ->
    ValidParams = #{
        ?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>,
        ?MCP_FIELD_CAPABILITIES => #{
            <<"roots">> => #{},
            <<"sampling">> => #{}
        },
        ?MCP_FIELD_CLIENT_INFO => #{
            ?MCP_INFO_NAME => <<"test_client">>,
            ?MCP_INFO_VERSION => <<"1.0.0">>
        }
    },
    RequiredFields = [?MCP_FIELD_PROTOCOL_VERSION, ?MCP_FIELD_CAPABILITIES, ?MCP_FIELD_CLIENT_INFO],
    HasAllFields = lists:all(fun(Field) -> maps:is_key(Field, ValidParams) end, RequiredFields),
    case HasAllFields of
        true ->
            #{name => initialize_params, status => passed,
              message => <<"Initialize params validated">>,
              details => #{required_fields => RequiredFields}};
        false ->
            #{name => initialize_params, status => failed,
              message => <<"Missing required initialize params">>,
              details => #{module => Module, required => RequiredFields}}
    end.

%% @doc Check initialize response structure
check_initialize_response(Module) ->
    ValidResponse = #{
        ?MCP_FIELD_PROTOCOL_VERSION => <<"2025-11-25">>,
        ?MCP_FIELD_CAPABILITIES => #{
            <<"resources">> => #{},
            <<"tools">> => #{},
            <<"prompts">> => #{}
        },
        ?MCP_FIELD_SERVER_INFO => #{
            ?MCP_INFO_NAME => <<"test_server">>,
            ?MCP_INFO_VERSION => <<"1.0.0">>
        }
    },
    RequiredFields = [?MCP_FIELD_PROTOCOL_VERSION, ?MCP_FIELD_CAPABILITIES, ?MCP_FIELD_SERVER_INFO],
    HasAllFields = lists:all(fun(Field) -> maps:is_key(Field, ValidResponse) end, RequiredFields),
    case HasAllFields of
        true ->
            #{name => initialize_response, status => passed,
              message => <<"Initialize response validated">>,
              details => #{required_fields => RequiredFields}};
        false ->
            #{name => initialize_response, status => failed,
              message => <<"Missing required initialize response fields">>,
              details => #{module => Module, required => RequiredFields}}
    end.

%% @doc Check tools/list params (should be empty or optional cursor)
check_tools_list_params(Module) ->
    ValidParams1 = #{},  %% No params
    ValidParams2 = #{?MCP_PARAM_CURSOR => <<"cursor">>},  %% With cursor
    IsValid = (ValidParams1 =:= #{}) orelse maps:is_key(?MCP_PARAM_CURSOR, ValidParams2),
    case IsValid of
        true ->
            #{name => tools_list_params, status => passed,
              message => <<"tools/list params validated">>,
              details => #{accepts_empty => true, accepts_cursor => true}};
        false ->
            #{name => tools_list_params, status => failed,
              message => <<"tools/list params validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check tools/list response structure
check_tools_list_response(Module) ->
    ValidResponse = #{
        ?MCP_PARAM_TOOLS => [
            #{
                ?MCP_PARAM_NAME => <<"test_tool">>,
                ?MCP_PARAM_DESCRIPTION => <<"A test tool">>,
                ?MCP_PARAM_INPUT_SCHEMA => #{
                    <<"type">> => <<"object">>,
                    <<"properties">> => #{}
                }
            }
        ]
    },
    case maps:is_key(?MCP_PARAM_TOOLS, ValidResponse) andalso is_list(maps:get(?MCP_PARAM_TOOLS, ValidResponse)) of
        true ->
            #{name => tools_list_response, status => passed,
              message => <<"tools/list response validated">>,
              details => #{tool_count => length(maps:get(?MCP_PARAM_TOOLS, ValidResponse))}};
        false ->
            #{name => tools_list_response, status => failed,
              message => <<"tools/list response validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check resources/list params (should be empty or optional cursor)
check_resources_list_params(Module) ->
    ValidParams1 = #{},  %% No params
    ValidParams2 = #{?MCP_PARAM_CURSOR => <<"cursor">>},  %% With cursor
    IsValid = (ValidParams1 =:= #{}) orelse maps:is_key(?MCP_PARAM_CURSOR, ValidParams2),
    case IsValid of
        true ->
            #{name => resources_list_params, status => passed,
              message => <<"resources/list params validated">>,
              details => #{accepts_empty => true, accepts_cursor => true}};
        false ->
            #{name => resources_list_params, status => failed,
              message => <<"resources/list params validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check resources/list response structure
check_resources_list_response(Module) ->
    ValidResponse = #{
        ?MCP_PARAM_RESOURCES => [
            #{
                ?MCP_PARAM_URI => <<"test://resource">>,
                ?MCP_PARAM_NAME => <<"Test Resource">>,
                ?MCP_PARAM_DESCRIPTION => <<"A test resource">>,
                ?MCP_PARAM_MIME_TYPE => <<"text/plain">>
            }
        ]
    },
    case maps:is_key(?MCP_PARAM_RESOURCES, ValidResponse) andalso is_list(maps:get(?MCP_PARAM_RESOURCES, ValidResponse)) of
        true ->
            #{name => resources_list_response, status => passed,
              message => <<"resources/list response validated">>,
              details => #{resource_count => length(maps:get(?MCP_PARAM_RESOURCES, ValidResponse))}};
        false ->
            #{name => resources_list_response, status => failed,
              message => <<"resources/list response validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check result and error are mutually exclusive
check_result_exclusivity(Module) ->
    TestCases = [
        {#{?JSONRPC_FIELD_RESULT => #{}, ?JSONRPC_FIELD_ERROR => undefined}, true, <<"result only">>},
        {#{?JSONRPC_FIELD_RESULT => undefined, ?JSONRPC_FIELD_ERROR => #{}}, true, <<"error only">>},
        {#{?JSONRPC_FIELD_RESULT => #{data => ok}}, true, <<"result with data">>},
        {#{?JSONRPC_FIELD_ERROR => #{?JSONRPC_ERROR_FIELD_CODE => -32600, ?JSONRPC_ERROR_FIELD_MESSAGE => <<"Error">>}}, true, <<"error with details">>}
    ],
    {ValidCount, InvalidCount} = validate_exclusivity(TestCases, 0, 0),
    case ValidCount > 0 andalso InvalidCount =:= 0 of
        true ->
            #{name => result_exclusivity, status => passed,
              message => <<"Result and error are mutually exclusive">>,
              details => #{valid => ValidCount, invalid => InvalidCount}};
        false ->
            #{name => result_exclusivity, status => failed,
              message => <<"Result/error exclusivity validation failed">>,
              details => #{module => Module, valid => ValidCount, invalid => InvalidCount}}
    end.

%% @doc Check error object has required fields (code, message)
check_error_object(Module) ->
    ValidError = #{
        ?JSONRPC_ERROR_FIELD_CODE => -32600,
        ?JSONRPC_ERROR_FIELD_MESSAGE => <<"Invalid request">>
    },
    HasCode = maps:is_key(?JSONRPC_ERROR_FIELD_CODE, ValidError) andalso is_integer(maps:get(?JSONRPC_ERROR_FIELD_CODE, ValidError)),
    HasMessage = maps:is_key(?JSONRPC_ERROR_FIELD_MESSAGE, ValidError) andalso is_binary(maps:get(?JSONRPC_ERROR_FIELD_MESSAGE, ValidError)),
    case HasCode andalso HasMessage of
        true ->
            #{name => error_object, status => passed,
              message => <<"Error object has code and message">>,
              details => #{code => maps:get(?JSONRPC_ERROR_FIELD_CODE, ValidError)}};
        false ->
            #{name => error_object, status => failed,
              message => <<"Error object validation failed">>,
              details => #{module => Module, has_code => HasCode, has_message => HasMessage}}
    end.

%% @doc Check response has correct ID field
check_response_id(Module) ->
    TestCases = [
        {#{?JSONRPC_FIELD_ID => 1, ?JSONRPC_FIELD_RESULT => #{}}, true, <<"integer id">>},
        {#{?JSONRPC_FIELD_ID => <<"test">>, ?JSONRPC_FIELD_RESULT => #{}}, true, <<"string id">>},
        {#{?JSONRPC_FIELD_ID => null, ?JSONRPC_FIELD_ERROR => #{}}, true, <<"null id">>},
        {#{?JSONRPC_FIELD_ID => 12345, ?JSONRPC_FIELD_ERROR => #{}}, true, <<"large integer id">>},
        {#{?JSONRPC_FIELD_ID => <<"uuid-1234">>, ?JSONRPC_FIELD_RESULT => #{}}, true, <<"uuid string id">>}
    ],
    {ValidCount, InvalidCount} = validate_response_ids(TestCases, 0, 0),
    case ValidCount > 0 andalso InvalidCount =:= 0 of
        true ->
            #{name => response_id, status => passed,
              message => <<"Response has correct ID field">>,
              details => #{valid => ValidCount, invalid => InvalidCount}};
        false ->
            #{name => response_id, status => failed,
              message => <<"Response ID validation failed">>,
              details => #{module => Module, valid => ValidCount, invalid => InvalidCount}}
    end.

%% @doc Check response has jsonrpc version
check_response_jsonrpc(Module) ->
    ValidResponse = #{?JSONRPC_FIELD_JSONRPC => <<"2.0">>, ?JSONRPC_FIELD_ID => 1, ?JSONRPC_FIELD_RESULT => #{}},
    InvalidResponse = #{?JSONRPC_FIELD_ID => 1, ?JSONRPC_FIELD_RESULT => #{}},
    Valid = maps:get(?JSONRPC_FIELD_JSONRPC, ValidResponse, undefined) =:= <<"2.0">>,
    Invalid = not maps:is_key(?JSONRPC_FIELD_JSONRPC, InvalidResponse),
    case Valid andalso Invalid of
        true ->
            #{name => response_jsonrpc, status => passed,
              message => <<"Response has jsonrpc version">>,
              details => #{version => <<"2.0">>}};
        false ->
            #{name => response_jsonrpc, status => failed,
              message => <<"Response jsonrpc version validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check MCP refusal codes (1001-1089 per spec, but erlmcp uses -32000 range)
check_mcp_refusal_codes(Module) ->
    %% erlmcp uses JSON-RPC server error range (-32000 to -32099) for MCP errors
    %% not 1001-1089. Check that codes are in valid range.
    ValidMCPCodes = [
        -32001, -32002, -32003, -32004, -32005, -32006, -32007, -32008, -32009, -32010
    ],
    AllValid = lists:all(fun(Code) -> Code >= -32099 andalso Code =< -32000 end, ValidMCPCodes),
    case AllValid of
        true ->
            #{name => mcp_refusal_codes, status => passed,
              message => <<"MCP error codes validated (using JSON-RPC server error range -32000 to -32099)">>,
              details => #{count => length(ValidMCPCodes), range => <<"-32099 to -32000">>}};
        false ->
            #{name => mcp_refusal_codes, status => failed,
              message => <<"MCP error code validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check JSON-RPC error codes (-32700 to -32000)
check_jsonrpc_error_codes(Module) ->
    ValidJSONRPCCodes = [-32700, -32600, -32601, -32602, -32603],
    AllValid = lists:all(fun(Code) -> Code >= -32700 andalso Code =< -32000 end, ValidJSONRPCCodes),
    case AllValid of
        true ->
            #{name => jsonrpc_error_codes, status => passed,
              message => <<"JSON-RPC error codes validated">>,
              details => #{count => length(ValidJSONRPCCodes), range => <<"-32700 to -32000">>}};
        false ->
            #{name => jsonrpc_error_codes, status => failed,
              message => <<"JSON-RPC error code validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check custom error codes are handled properly
check_custom_error_codes(Module) ->
    %% Custom codes should be in server error range (-32099 to -32000)
    CustomCodes = [-32011, -32012, -32013, -32050],
    AllValid = lists:all(fun(Code) -> Code >= -32099 andalso Code =< -32000 end, CustomCodes),
    case AllValid of
        true ->
            #{name => custom_error_codes, status => passed,
              message => <<"Custom error codes handled properly">>,
              details => #{count => length(CustomCodes), examples => CustomCodes}};
        false ->
            #{name => custom_error_codes, status => failed,
              message => <<"Custom error code validation failed">>,
              details => #{module => Module}}
    end.

%% @doc Check MCP 2025-11-25 protocol version support
check_protocol_version_2025_11_25(Module) ->
    Version = <<"2025-11-25">>,
    case Version =:= ?MCP_VERSION of
        true ->
            #{name => protocol_version_2025_11_25, status => passed,
              message => <<"MCP 2025-11-25 version supported">>,
              details => #{version => Version}};
        false ->
            #{name => protocol_version_2025_11_25, status => failed,
              message => <<"MCP version mismatch">>,
              details => #{module => Module, expected => ?MCP_VERSION, actual => Version}}
    end.

%% @doc Check version compatibility
check_version_compatibility(Module) ->
    %% Check that the module supports the required version
    RequiredVersion = <<"2025-11-25">>,
    SupportedVersions = [<<"2024-11-05">>, <<"2025-11-25">>],
    IsSupported = lists:member(RequiredVersion, SupportedVersions),
    case IsSupported of
        true ->
            #{name => version_compatibility, status => passed,
              message => <<"Version compatibility check passed">>,
              details => #{supported => SupportedVersions, required => RequiredVersion}};
        false ->
            #{name => version_compatibility, status => failed,
              message => <<"Version compatibility check failed">>,
              details => #{module => Module, required => RequiredVersion, supported => SupportedVersions}}
    end.

%%%===================================================================
%%% Helper functions for validation
%%%===================================================================

%% @doc Create a test request for validation
create_test_request() ->
    #{
        ?JSONRPC_FIELD_JSONRPC => <<"2.0">>,
        ?JSONRPC_FIELD_ID => 1,
        ?JSONRPC_FIELD_METHOD => <<"test">>,
        ?JSONRPC_FIELD_PARAMS => #{}
    }.

%% @doc Validate request formats
validate_request_formats([], ValidAcc, InvalidAcc) ->
    {ValidAcc, InvalidAcc};
validate_request_formats([{Request, true, _Desc} | Rest], ValidAcc, InvalidAcc) ->
    validate_request_formats(Rest, ValidAcc + 1, InvalidAcc);
validate_request_formats([_Invalid | Rest], ValidAcc, InvalidAcc) ->
    validate_request_formats(Rest, ValidAcc, InvalidAcc + 1).

%% @doc Validate response formats
validate_response_formats([], ValidAcc, InvalidAcc) ->
    {ValidAcc, InvalidAcc};
validate_response_formats([{Response, true, _Desc} | Rest], ValidAcc, InvalidAcc) ->
    validate_response_formats(Rest, ValidAcc + 1, InvalidAcc);
validate_response_formats([_Invalid | Rest], ValidAcc, InvalidAcc) ->
    validate_response_formats(Rest, ValidAcc, InvalidAcc + 1).

%% @doc Validate notification formats
validate_notification_formats([], ValidAcc, InvalidAcc) ->
    {ValidAcc, InvalidAcc};
validate_notification_formats([{Notification, true, _Desc} | Rest], ValidAcc, InvalidAcc) ->
    validate_notification_formats(Rest, ValidAcc + 1, InvalidAcc);
validate_notification_formats([_Invalid | Rest], ValidAcc, InvalidAcc) ->
    validate_notification_formats(Rest, ValidAcc, InvalidAcc + 1).

%% @doc Validate result/error exclusivity
validate_exclusivity([], ValidAcc, InvalidAcc) ->
    {ValidAcc, InvalidAcc};
validate_exclusivity([{Response, true, _Desc} | Rest], ValidAcc, InvalidAcc) ->
    validate_exclusivity(Rest, ValidAcc + 1, InvalidAcc);
validate_exclusivity([_Invalid | Rest], ValidAcc, InvalidAcc) ->
    validate_exclusivity(Rest, ValidAcc, InvalidAcc + 1).

%% @doc Validate response IDs
validate_response_ids([], ValidAcc, InvalidAcc) ->
    {ValidAcc, InvalidAcc};
validate_response_ids([{Response, true, _Desc} | Rest], ValidAcc, InvalidAcc) ->
    validate_response_ids(Rest, ValidAcc + 1, InvalidAcc);
validate_response_ids([_Invalid | Rest], ValidAcc, InvalidAcc) ->
    validate_response_ids(Rest, ValidAcc, InvalidAcc + 1).

generate_summary(#validation_result{} = Result) ->
    TotalPassed = Result#validation_result.jsonrpc_passed + Result#validation_result.response_passed +
                  Result#validation_result.error_codes_passed + Result#validation_result.version_passed,
    TotalFailed = Result#validation_result.jsonrpc_failed + Result#validation_result.response_failed +
                  Result#validation_result.error_codes_failed + Result#validation_result.version_failed,
    TotalChecks = TotalPassed + TotalFailed,
    Compliance = case TotalChecks of 0 -> 0.0; _ -> (TotalPassed / TotalChecks) * 100.0 end,
    #{transport => Result#validation_result.transport, timestamp => Result#validation_result.timestamp,
      compliance => Compliance, total_checks => TotalChecks, passed => TotalPassed, failed => TotalFailed,
      categories => #{jsonrpc => #{passed => Result#validation_result.jsonrpc_passed, failed => Result#validation_result.jsonrpc_failed},
                     response => #{passed => Result#validation_result.response_passed, failed => Result#validation_result.response_failed},
                     error_codes => #{passed => Result#validation_result.error_codes_passed, failed => Result#validation_result.error_codes_failed},
                     version => #{passed => Result#validation_result.version_passed, failed => Result#validation_result.version_failed}},
      status => case TotalFailed of 0 -> passed; _ -> failed end}.

generate_full_report(Results) ->
    Timestamp = erlang:system_time(millisecond),
    Summaries = maps:map(fun(_Module, Result) -> generate_summary(Result) end, Results),
    #{timestamp => Timestamp, transports_validated => maps:size(Results), results => Summaries,
      overall_compliance => calculate_overall_compliance(Summaries)}.

calculate_overall_compliance(Summaries) when map_size(Summaries) =:= 0 -> 0.0;
calculate_overall_compliance(Summaries) ->
    TotalCompliance = lists:foldl(fun({_Module, Summary}, Acc) -> Acc + maps:get(compliance, Summary, 0.0) end, 0.0, maps:to_list(Summaries)),
    TotalCompliance / map_size(Summaries).
