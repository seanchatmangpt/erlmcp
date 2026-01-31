%%%-------------------------------------------------------------------
%%% @doc erlmcp_spec_parser Tests - Enhanced Version
%%%
%%% Comprehensive test suite for erlmcp_spec_parser module with 25+ tests.
%%% Tests spec parsing, method requirements, error codes, transports,
%%% capabilities, and validation functions.
%%%
%%% Chicago School TDD:
%%% - Real erlmcp_spec_parser gen_server (no mocks)
%%% - State-based verification (assert on actual returned data)
%%% - Test observable behavior through API
%%%
%%% Coverage target: 85%+ (core validation module)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_spec_parser_tests_enhanced).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp_spec_parser.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% Setup/Teardown for spec parser tests
spec_parser_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Spec Version and Constants", fun test_spec_version_constants/0},
      {"Spec Parsing Structure", fun test_spec_parsing_structure/0},
      {"All Methods Parsed", fun test_all_methods_parsed/0},
      {"All Error Codes Parsed", fun test_all_error_codes_parsed/0},
      {"All Transports Parsed", fun test_all_transports_parsed/0},
      {"All Capabilities Parsed", fun test_all_capabilities_parsed/0},

      %% Method tests
      {"Initialize Method Complete", fun test_initialize_method_complete/0},
      {"Tools List Method", fun test_tools_list_method/0},
      {"Tools Call Method", fun test_tools_call_method/0},
      {"Resources Read Method", fun test_resources_read_method/0},
      {"Resources List Method", fun test_resources_list_method/0},
      {"Prompts List Method", fun test_prompts_list_method/0},
      {"Prompts Get Method", fun test_prompts_get_method/0},
      {"Ping Method", fun test_ping_method/0},

      %% Error code tests
      {"JSON-RPC Error Codes Valid Range", fun test_jsonrpc_error_codes_range/0},
      {"MCP Protocol Error Codes", fun test_mcp_protocol_error_codes/0},
      {"Refusal Codes 1001-1089", fun test_refusal_codes_range/0},
      {"Error Code Categories", fun test_error_code_categories/0},
      {"Error Retry Strategies", fun test_error_retry_strategies/0},

      %% Transport tests
      {"STDIO Transport Complete", fun test_stdio_transport_complete/0},
      {"SSE Transport Complete", fun test_sse_transport_complete/0},
      {"Transport Features", fun test_transport_features/0},

      %% Capability tests
      {"Resources Capability", fun test_resources_capability/0},
      {"Tools Capability", fun test_tools_capability/0},
      {"Prompts Capability", fun test_prompts_capability/0},
      {"Sampling Capability", fun test_sampling_capability/0},

      %% Validation tests
      {"Validate Valid Message", fun test_validate_valid_message/0},
      {"Validate Invalid JSON-RPC Version", fun test_validate_invalid_version/0},
      {"Validate Missing Required Fields", fun test_validate_missing_fields/0},
      {"Validate Method Call Parameters", fun test_validate_method_params/0},
      {"Validate Error Codes", fun test_validate_error_codes/0},
      {"Check Capability Support", fun test_check_capability_support/0},
      {"Generate Validation Rules", fun test_generate_validation_rules/0}
     ]}.

%% Setup function - start the spec parser gen_server
setup() ->
    {ok, Pid} = erlmcp_spec_parser:start_link(),
    Pid.

%% Cleanup function - stop the spec parser gen_server
cleanup(_Pid) ->
    erlmcp_spec_parser:stop(),
    timer:sleep(50),
    %% Verify process stopped
    undefined = whereis(erlmcp_spec_parser).

%%%====================================================================
%%% Spec Version and Constants Tests
%%%====================================================================

test_spec_version_constants() ->
    %% Test version constant
    ?assertEqual(<<"2025-11-25">>, erlmcp_spec_parser:spec_version()),

    %% Test spec returns correct version
    {ok, Spec} = erlmcp_spec_parser:get_spec(),
    ?assertEqual(<<"2025-11-25">>, Spec#mcp_spec.version),
    ?assertEqual(<<"2025-11-25">>, Spec#mcp_spec.specification_date),
    ?assertEqual(<<"JSON-RPC 2.0">>, Spec#mcp_spec.protocol_type).

test_spec_parsing_structure() ->
    {ok, Spec} = erlmcp_spec_parser:parse_spec(),
    ?assert(is_record(Spec, mcp_spec)),
    ?assert(is_list(Spec#mcp_spec.methods)),
    ?assert(is_list(Spec#mcp_spec.error_codes)),
    ?assert(is_list(Spec#mcp_spec.transports)),
    ?assert(is_list(Spec#mcp_spec.capabilities)),

    %% Verify all fields are populated
    ?assert(length(Spec#mcp_spec.methods) > 0),
    ?assert(length(Spec#mcp_spec.error_codes) > 0),
    ?assert(length(Spec#mcp_spec.transports) > 0),
    ?assert(length(Spec#mcp_spec.capabilities) > 0).

test_all_methods_parsed() ->
    {ok, Methods} = erlmcp_spec_parser:list_methods(),
    ?assert(is_list(Methods)),
    ?assert(length(Methods) >= 16),  %% At least 16 methods in spec

    %% Verify key methods exist
    ?assert(lists:member(<<"initialize">>, Methods)),
    ?assert(lists:member(<<"tools/list">>, Methods)),
    ?assert(lists:member(<<"tools/call">>, Methods)),
    ?assert(lists:member(<<"resources/list">>, Methods)),
    ?assert(lists:member(<<"resources/read">>, Methods)),
    ?assert(lists:member(<<"prompts/list">>, Methods)),
    ?assert(lists:member(<<"prompts/get">>, Methods)),
    ?assert(lists:member(<<"ping">>, Methods)).

test_all_error_codes_parsed() ->
    {ok, ErrorCodes} = erlmcp_spec_parser:list_error_codes(),
    ?assert(is_list(ErrorCodes)),
    ?assert(length(ErrorCodes) >= 94),  %% 5 JSON-RPC + 89 refusal codes

    %% Verify JSON-RPC standard error codes
    ?assert(lists:member(-32700, ErrorCodes)),  %% Parse error
    ?assert(lists:member(-32600, ErrorCodes)),  %% Invalid Request
    ?assert(lists:member(-32601, ErrorCodes)),  %% Method not found
    ?assert(lists:member(-32602, ErrorCodes)),  %% Invalid params
    ?assert(lists:member(-32603, ErrorCodes)),  %% Internal error

    %% Verify MCP error codes
    ?assert(lists:member(-32001, ErrorCodes)),  %% Resource not found
    ?assert(lists:member(-32002, ErrorCodes)),  %% Tool not found

    %% Verify refusal codes (1001-1089)
    ?assert(lists:member(1001, ErrorCodes)),
    ?assert(lists:member(1089, ErrorCodes)).

test_all_transports_parsed() ->
    {ok, Transports} = erlmcp_spec_parser:list_transports(),
    ?assert(is_list(Transports)),
    ?assert(length(Transports) >= 3),

    %% Verify key transports
    ?assert(lists:member(<<"stdio">>, Transports)),
    ?assert(lists:member(<<"stdio-transport">>, Transports)),
    ?assert(lists:member(<<"sse">>, Transports)).

test_all_capabilities_parsed() ->
    {ok, Capabilities} = erlmcp_spec_parser:list_capabilities(),
    ?assert(is_list(Capabilities)),
    ?assert(length(Capabilities) >= 6),

    %% Verify key capabilities
    ?assert(lists:member(<<"resources">>, Capabilities)),
    ?assert(lists:member(<<"tools">>, Capabilities)),
    ?assert(lists:member(<<"prompts">>, Capabilities)),
    ?assert(lists:member(<<"logging">>, Capabilities)),
    ?assert(lists:member(<<"tasks">>, Capabilities)),
    ?assert(lists:member(<<"sampling">>, Capabilities)).

%%%====================================================================
%%% Method Tests
%%%====================================================================

test_initialize_method_complete() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"initialize">>),
    ?assertEqual(<<"initialize">>, Method#method_req.name),
    ?assertEqual(notification, Method#method_req.method_type),
    ?assertEqual(client_to_server, Method#method_req.direction),
    ?assertEqual(true, Method#method_req.required),
    ?assertEqual(stable, Method#method_req.deprecation_status),

    %% Verify params
    ParamSpec = Method#method_req.params_spec,
    ?assert(maps:is_key(protocolVersion, ParamSpec)),
    ?assert(maps:is_key(capabilities, ParamSpec)),
    ?assert(maps:is_key(clientInfo, ParamSpec)).

test_tools_list_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"tools/list">>),
    ?assertEqual(request, Method#method_req.method_type),
    ?assertEqual(client_to_server, Method#method_req.direction),
    ?assertEqual(<<"tools">>, Method#method_req.capability_required),

    %% Verify result spec
    ResultSpec = Method#method_req.result_spec,
    ?assert(is_map(ResultSpec)),
    ?assert(maps:is_key(tools, ResultSpec)).

test_tools_call_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"tools/call">>),
    ?assertEqual(request, Method#method_req.method_type),

    %% Verify required params
    ParamSpec = Method#method_req.params_spec,
    ?assert(maps:is_key(name, ParamSpec)),
    ?assert(maps:is_key(arguments, ParamSpec)),

    Name = maps:get(name, ParamSpec),
    ?assertEqual(true, maps:get(required, Name)),

    Args = maps:get(arguments, ParamSpec),
    ?assertEqual(true, maps:get(required, Args)).

test_resources_read_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"resources/read">>),
    ?assertEqual(request, Method#method_req.method_type),
    ?assertEqual(<<"resources">>, Method#method_req.capability_required),

    %% Verify URI parameter
    ParamSpec = Method#method_req.params_spec,
    ?assert(maps:is_key(uri, ParamSpec)),
    Uri = maps:get(uri, ParamSpec),
    ?assertEqual(true, maps:get(required, Uri)).

test_resources_list_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"resources/list">>),
    ?assertEqual(request, Method#method_req.method_type),
    ?assertEqual(<<"resources">>, Method#method_req.capability_required),

    %% Verify optional cursor param
    ParamSpec = Method#method_req.params_spec,
    ?assert(maps:is_key(cursor, ParamSpec)).

test_prompts_list_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"prompts/list">>),
    ?assertEqual(request, Method#method_req.method_type),
    ?assertEqual(<<"prompts">>, Method#method_req.capability_required).

test_prompts_get_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"prompts/get">>),
    ?assertEqual(request, Method#method_req.method_type),

    ParamSpec = Method#method_req.params_spec,
    ?assert(maps:is_key(name, ParamSpec)),
    Name = maps:get(name, ParamSpec),
    ?assertEqual(true, maps:get(required, Name)).

test_ping_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"ping">>),
    ?assertEqual(request, Method#method_req.method_type),
    ?assertEqual(undefined, Method#method_req.capability_required).

%%%====================================================================
%%% Error Code Tests
%%%====================================================================

test_jsonrpc_error_codes_range() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),
    JsonRpcErrors = [E || E <- Errors, E#error_code_req.category =:= json_rpc],

    ?assert(length(JsonRpcErrors) >= 5),

    %% Verify all in valid range
    lists:foreach(fun(E) ->
        Code = E#error_code_req.code,
        ?assert(Code >= -32700 andalso Code =< -32600)
    end, JsonRpcErrors).

test_mcp_protocol_error_codes() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),
    McpErrors = [E || E <- Errors,
                      E#error_code_req.category =:= mcp_protocol,
                      E#error_code_req.code < 0],

    ?assert(length(McpErrors) >= 3),

    %% Verify specific codes
    {ok, ResourceNotFound} = erlmcp_spec_parser:get_error_requirements(-32001),
    ?assertEqual(<<"Resource not found">>, ResourceNotFound#error_code_req.name),

    {ok, ToolNotFound} = erlmcp_spec_parser:get_error_requirements(-32002),
    ?assertEqual(<<"Tool not found">>, ToolNotFound#error_code_req.name).

test_refusal_codes_range() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),
    RefusalCodes = [E || E <- Errors,
                         E#error_code_req.code >= 1001,
                         E#error_code_req.code =< 1089],

    ?assertEqual(89, length(RefusalCodes)),  %% Should have all 89 refusal codes

    %% Verify first and last
    {ok, First} = erlmcp_spec_parser:get_error_requirements(1001),
    ?assertEqual(1001, First#error_code_req.code),

    {ok, Last} = erlmcp_spec_parser:get_error_requirements(1089),
    ?assertEqual(1089, Last#error_code_req.code).

test_error_code_categories() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),

    %% Group by category
    Categories = lists:foldl(fun(E, Acc) ->
        Cat = E#error_code_req.category,
        maps:update_with(Cat, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, Errors),

    ?assert(maps:is_key(json_rpc, Categories)),
    ?assert(maps:is_key(mcp_protocol, Categories)),
    ?assert(maps:get(json_rpc, Categories) >= 5),
    ?assert(maps:get(mcp_protocol, Categories) >= 92).

test_error_retry_strategies() ->
    %% Test abort strategy
    {ok, ParseError} = erlmcp_spec_parser:get_error_requirements(-32700),
    ?assertEqual(abort, ParseError#error_code_req.retry_strategy),

    %% Test retry strategy
    {ok, InternalError} = erlmcp_spec_parser:get_error_requirements(-32603),
    ?assertEqual(retry, InternalError#error_code_req.retry_strategy),

    %% Test refusal code retry strategy
    {ok, RateLimited} = erlmcp_spec_parser:get_error_requirements(1063),
    ?assertEqual(retry, RateLimited#error_code_req.retry_strategy).

%%%====================================================================
%%% Transport Tests
%%%====================================================================

test_stdio_transport_complete() ->
    {ok, Transport} = erlmcp_spec_parser:get_transport_requirements(<<"stdio">>),
    ?assertEqual(<<"stdio">>, Transport#transport_req.name),
    ?assertEqual(stream_based, Transport#transport_req.transport_type),
    ?assertEqual(json_delimiter, Transport#transport_req.framing),
    ?assertEqual(true, Transport#transport_req.connection_oriented),
    ?assertEqual(false, Transport#transport_req.multiplexing_support),

    %% Verify required features
    RequiredFeatures = Transport#transport_req.required_features,
    ?assert(lists:member(<<"newline_delimiter">>, RequiredFeatures)).

test_sse_transport_complete() ->
    {ok, Transport} = erlmcp_spec_parser:get_transport_requirements(<<"sse">>),
    ?assertEqual(<<"sse">>, Transport#transport_req.name),
    ?assertEqual(stream_based, Transport#transport_req.transport_type),
    ?assertEqual(true, Transport#transport_req.multiplexing_support),

    %% Verify features
    RequiredFeatures = Transport#transport_req.required_features,
    ?assert(lists:member(<<"http_sse">>, RequiredFeatures)),

    OptionalFeatures = Transport#transport_req.optional_features,
    ?assert(lists:member(<<"compression">>, OptionalFeatures)).

test_transport_features() ->
    {ok, Transports} = erlmcp_spec_parser:get_transport_requirements(),

    %% All transports should have required_features
    lists:foreach(fun(T) ->
        ?assert(is_list(T#transport_req.required_features)),
        ?assert(is_list(T#transport_req.optional_features))
    end, Transports).

%%%====================================================================
%%% Capability Tests
%%%====================================================================

test_resources_capability() ->
    {ok, Cap} = erlmcp_spec_parser:get_capability_requirements(<<"resources">>),
    ?assertEqual(<<"resources">>, Cap#capability_req.name),
    ?assertEqual(server, Cap#capability_req.category),
    ?assertEqual(false, Cap#capability_req.required),

    Features = Cap#capability_req.features,
    ?assert(lists:member(<<"subscribe">>, Features)),
    ?assert(lists:member(<<"list">>, Features)),
    ?assert(lists:member(<<"read">>, Features)).

test_tools_capability() ->
    {ok, Cap} = erlmcp_spec_parser:get_capability_requirements(<<"tools">>),
    ?assertEqual(server, Cap#capability_req.category),

    Features = Cap#capability_req.features,
    ?assert(lists:member(<<"list">>, Features)),
    ?assert(lists:member(<<"call">>, Features)).

test_prompts_capability() ->
    {ok, Cap} = erlmcp_spec_parser:get_capability_requirements(<<"prompts">>),
    ?assertEqual(server, Cap#capability_req.category),

    Features = Cap#capability_req.features,
    ?assert(lists:member(<<"list">>, Features)),
    ?assert(lists:member(<<"get">>, Features)).

test_sampling_capability() ->
    {ok, Cap} = erlmcp_spec_parser:get_capability_requirements(<<"sampling">>),
    ?assertEqual(client, Cap#capability_req.category),
    ?assertEqual(false, Cap#capability_req.required),

    Features = Cap#capability_req.features,
    ?assert(lists:member(<<"complete">>, Features)).

%%%====================================================================
%%% Validation Tests
%%%====================================================================

test_validate_valid_message() ->
    ValidMsg = #{
        jsonrpc => <<"2.0">>,
        method => <<"test">>,
        id => 1
    },
    ?assertEqual({ok, valid_message}, erlmcp_spec_parser:validate_message(ValidMsg)).

test_validate_invalid_version() ->
    InvalidMsg = #{
        jsonrpc => <<"1.0">>,
        method => <<"test">>,
        id => 1
    },
    ?assertEqual({error, invalid_jsonrpc_version},
                 erlmcp_spec_parser:validate_message(InvalidMsg)).

test_validate_missing_fields() ->
    MissingMethod = #{jsonrpc => <<"2.0">>, id => 1},
    ?assertEqual({error, missing_required_fields},
                 erlmcp_spec_parser:validate_message(MissingMethod)),

    MissingJsonRpc = #{method => <<"test">>, id => 1},
    ?assertEqual({error, missing_required_fields},
                 erlmcp_spec_parser:validate_message(MissingJsonRpc)).

test_validate_method_params() ->
    %% Valid params
    ValidParams = #{name => <<"tool">>, arguments => #{}},
    ?assertEqual({ok, valid_params},
                 erlmcp_spec_parser:validate_method_call(<<"tools/call">>, ValidParams)),

    %% Missing required parameter
    InvalidParams = #{name => <<"tool">>},
    ?assertMatch({error, {missing_params, _}},
                 erlmcp_spec_parser:validate_method_call(<<"tools/call">>, InvalidParams)),

    %% Unknown method
    ?assertEqual({error, method_not_found},
                 erlmcp_spec_parser:validate_method_call(<<"unknown">>, #{})).

test_validate_error_codes() ->
    %% Valid codes
    ?assertEqual({ok, valid_error_code},
                 erlmcp_spec_parser:validate_error_code(-32700)),
    ?assertEqual({ok, valid_error_code},
                 erlmcp_spec_parser:validate_error_code(1001)),

    %% Invalid code
    ?assertEqual({error, unknown_error_code},
                 erlmcp_spec_parser:validate_error_code(-99999)).

test_check_capability_support() ->
    %% Full support
    ?assertEqual({ok, capability_supported},
                 erlmcp_spec_parser:check_capability_support(
                     <<"tools">>, [<<"list">>, <<"call">>])),

    %% Partial support (missing features)
    ?assertMatch({error, {missing_features, _}},
                 erlmcp_spec_parser:check_capability_support(
                     <<"resources">>, [<<"list">>])),

    %% Unknown capability
    ?assertEqual({error, unknown_capability},
                 erlmcp_spec_parser:check_capability_support(
                     <<"unknown">>, [])).

test_generate_validation_rules() ->
    {ok, Rules} = erlmcp_spec_parser:generate_validation_rules(),
    ?assert(is_list(Rules)),
    ?assert(length(Rules) >= 4),

    %% Verify rule structure
    lists:foreach(fun(Rule) ->
        ?assert(is_record(Rule, validation_rule)),
        ?assert(is_binary(Rule#validation_rule.rule_id)),
        ?assert(is_atom(Rule#validation_rule.severity)),
        ?assert(is_atom(Rule#validation_rule.category)),
        ?assert(is_binary(Rule#validation_rule.error_message))
    end, Rules).
