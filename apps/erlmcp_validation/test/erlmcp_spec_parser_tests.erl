%%%-------------------------------------------------------------------
%%% @doc erlmcp_spec_parser Tests
%%%
%%% Comprehensive test suite for erlmcp_spec_parser module.
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
-module(erlmcp_spec_parser_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_spec_parser.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% Setup/Teardown for spec parser tests
spec_parser_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Spec Parsing Tests", {spawn, fun() -> [
          ?_test(parse_spec_returns_correct_structure()),
          ?_test(version_is_2025_11_25()),
          ?_test(methods_are_parsed_correctly()),
          ?_test(error_codes_are_parsed_correctly()),
          ?_test(transports_are_parsed_correctly())
      ] end}},
      {"Method Requirements Tests", {spawn, fun() -> [
          ?_test(get_method_requirements_returns_all_methods()),
          ?_test(get_method_requirements_returns_specific_method()),
          ?_test(initialize_method_has_correct_params()),
          ?_test(tools_call_method_has_correct_params()),
          ?_test(resources_read_method_has_correct_params())
      ] end}},
      {"Error Code Requirements Tests", {spawn, fun() -> [
          ?_test(get_error_requirements_returns_all_error_codes()),
          ?_test(get_error_requirements_returns_specific_error_code()),
          ?_test(json_rpc_error_codes_are_valid()),
          ?_test(mcp_error_codes_are_valid()),
          ?_test(refusal_codes_are_valid())
      ] end}},
      {"Validation Tests", {spawn, fun() -> [
          ?_test(validate_message_validates_json_rpc_structure()),
          ?_test(validate_method_call_validates_parameters()),
          ?_test(validate_error_code_validates_error_codes()),
          ?_test(check_capability_support_checks_capabilities()),
          ?_test(generate_validation_rules_returns_all_rules())
      ] end}},
      {"Transport Requirements Tests", {spawn, fun() -> [
          ?_test(get_transport_requirements_returns_all_transports()),
          ?_test(get_transport_requirements_returns_specific_transport()),
          ?_test(stdio_transport_has_correct_requirements())
      ] end}},
      {"Capability Requirements Tests", {spawn, fun() -> [
          ?_test(get_capability_requirements_returns_all_capabilities()),
          ?_test(get_capability_requirements_returns_specific_capability()),
          ?_test(resources_capability_has_correct_features())
      ] end}}
     ]}.

%% Setup function - start the spec parser gen_server
setup() ->
    {ok, Pid} = erlmcp_spec_parser:start_link(),
    Pid.

%% Cleanup function - stop the spec parser gen_server
cleanup(_Pid) ->
    erlmcp_spec_parser:stop(),
    %% Verify process stopped
    undefined = whereis(erlmcp_spec_parser).

%%%====================================================================
%%% 1. Spec Parsing Tests (5 tests)
%%%====================================================================

%% @doc Test that parse_spec returns correct #mcp_spec record structure
parse_spec_returns_correct_structure() ->
    {ok, Spec} = erlmcp_spec_parser:parse_spec(),
    ?assert(is_record(Spec, mcp_spec)),
    ?assertMatch(#mcp_spec{
        version = _,
        specification_date = _,
        protocol_type = _,
        methods = _,
        error_codes = _,
        transports = _,
        capabilities = _
    }, Spec).

%% @doc Test that version is correctly set to 2025-11-25
version_is_2025_11_25() ->
    {ok, Spec} = erlmcp_spec_parser:parse_spec(),
    ?assertEqual(<<"2025-11-25">>, Spec#mcp_spec.version),
    ?assertEqual(<<"2025-11-25">>, Spec#mcp_spec.specification_date),
    ?assertEqual(<<"JSON-RPC 2.0">>, Spec#mcp_spec.protocol_type).

%% @doc Test that methods are parsed correctly into #method_req records
methods_are_parsed_correctly() ->
    {ok, Spec} = erlmcp_spec_parser:parse_spec(),
    Methods = Spec#mcp_spec.methods,
    ?assert(is_list(Methods)),
    ?assert(length(Methods) >= 7),  %% At least 7 methods defined
    ?assert(is_record(hd(Methods), method_req)),
    %% Verify initialize method exists
    Initialize = lists:keyfind(<<"initialize">>, #method_req.name, Methods),
    ?assertNotEqual(false, Initialize).

%% @doc Test that error codes are parsed correctly into #error_code_req records
error_codes_are_parsed_correctly() ->
    {ok, Spec} = erlmcp_spec_parser:parse_spec(),
    Errors = Spec#mcp_spec.error_codes,
    ?assert(is_list(Errors)),
    ?assert(length(Errors) >= 10),  %% At least 10 error codes
    ?assert(is_record(hd(Errors), error_code_req)),
    %% Verify parse error exists
    ParseError = lists:keyfind(-32700, #error_code_req.code, Errors),
    ?assertNotEqual(false, ParseError).

%% @doc Test that transports are parsed correctly into #transport_req records
transports_are_parsed_correctly() ->
    {ok, Spec} = erlmcp_spec_parser:parse_spec(),
    Transports = Spec#mcp_spec.transports,
    ?assert(is_list(Transports)),
    ?assert(length(Transports) >= 3),  %% At least 3 transports
    ?assert(is_record(hd(Transports), transport_req)),
    %% Verify stdio transport exists
    Stdio = lists:keyfind(<<"stdio">>, #transport_req.name, Transports),
    ?assertNotEqual(false, Stdio).

%%%====================================================================
%%% 2. Method Requirements Tests (5 tests)
%%%====================================================================

%% @doc Test get_method_requirements/0 returns all methods
get_method_requirements_returns_all_methods() ->
    {ok, Methods} = erlmcp_spec_parser:get_method_requirements(),
    ?assert(is_list(Methods)),
    ?assert(length(Methods) >= 7),
    ?assert(is_record(hd(Methods), method_req)).

%% @doc Test get_method_requirements/1 returns specific method
get_method_requirements_returns_specific_method() ->
    {ok, Method} = erlmcp_spec_parser:get_method_requirements(<<"initialize">>),
    ?assertMatch(#method_req{name = <<"initialize">>}, Method),
    ?assertEqual(notification, Method#method_req.method_type),
    ?assertEqual(client_to_server, Method#method_req.direction),
    ?assertEqual(true, Method#method_req.required).

%% @doc Test that initialize method has correct parameters
initialize_method_has_correct_params() ->
    {ok, InitMethod} = erlmcp_spec_parser:get_method_requirements(<<"initialize">>),
    ParamSpec = InitMethod#method_req.params_spec,
    ?assert(is_map(ParamSpec)),
    ?assert(maps:is_key(protocolVersion, ParamSpec)),
    ?assert(maps:is_key(capabilities, ParamSpec)),
    ?assert(maps:is_key(clientInfo, ParamSpec)),
    %% Verify required fields
    ProtocolVer = maps:get(protocolVersion, ParamSpec),
    ?assertEqual(true, maps:get(required, ProtocolVer)).

%% @doc Test that tools/call method has correct parameters
tools_call_method_has_correct_params() ->
    {ok, ToolsCall} = erlmcp_spec_parser:get_method_requirements(<<"tools/call">>),
    ParamSpec = ToolsCall#method_req.params_spec,
    ?assert(is_map(ParamSpec)),
    ?assert(maps:is_key(name, ParamSpec)),
    ?assert(maps:is_key(arguments, ParamSpec)),
    %% Verify required fields
    Name = maps:get(name, ParamSpec),
    ?assertEqual(true, maps:get(required, Name)),
    %% Verify capability requirement
    ?assertEqual(<<"tools">>, ToolsCall#method_req.capability_required).

%% @doc Test that resources/read method has correct parameters
resources_read_method_has_correct_params() ->
    {ok, ResourcesRead} = erlmcp_spec_parser:get_method_requirements(<<"resources/read">>),
    ParamSpec = ResourcesRead#method_req.params_spec,
    ?assert(is_map(ParamSpec)),
    ?assert(maps:is_key(uri, ParamSpec)),
    %% Verify required fields
    Uri = maps:get(uri, ParamSpec),
    ?assertEqual(true, maps:get(required, Uri)),
    %% Verify capability requirement
    ?assertEqual(<<"resources">>, ResourcesRead#method_req.capability_required).

%%%====================================================================
%%% 3. Error Code Requirements Tests (5 tests)
%%%====================================================================

%% @doc Test get_error_requirements/0 returns all error codes
get_error_requirements_returns_all_error_codes() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),
    ?assert(is_list(Errors)),
    ?assert(length(Errors) >= 10),
    ?assert(is_record(hd(Errors), error_code_req)).

%% @doc Test get_error_requirements/1 returns specific error code
get_error_requirements_returns_specific_error_code() ->
    {ok, Error} = erlmcp_spec_parser:get_error_requirements(-32700),
    ?assertMatch(#error_code_req{code = -32700}, Error),
    ?assertEqual(<<"Parse error">>, Error#error_code_req.name),
    ?assertEqual(json_rpc, Error#error_code_req.category),
    ?assertEqual(error, Error#error_code_req.severity).

%% @doc Test that JSON-RPC error codes are valid (-32700 to -32603)
json_rpc_error_codes_are_valid() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),
    JsonRpcErrors = [E || E <- Errors, E#error_code_req.category =:= json_rpc],
    ?assert(length(JsonRpcErrors) >= 5),
    %% Verify all JSON-RPC codes are in valid range
    lists:foreach(fun(E) ->
        Code = E#error_code_req.code,
        ?assert(Code >= -32700 andalso Code =< -32600)
    end, JsonRpcErrors).

%% @doc Test that MCP error codes are valid (-32001 to -32010)
mcp_error_codes_are_valid() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),
    McpErrors = [E || E <- Errors,
                      E#error_code_req.category =:= mcp_protocol,
                      E#error_code_req.code < 0],
    ?assert(length(McpErrors) >= 3),
    %% Verify all MCP error codes are in valid range
    lists:foreach(fun(E) ->
        Code = E#error_code_req.code,
        ?assert(Code >= -32099 andalso Code =< -32000)
    end, McpErrors).

%% @doc Test that refusal codes are valid (1001-1089)
refusal_codes_are_valid() ->
    {ok, Errors} = erlmcp_spec_parser:get_error_requirements(),
    RefusalCodes = [E || E <- Errors,
                         E#error_code_req.category =:= mcp_protocol,
                         E#error_code_req.code >= 1000],
    ?assert(length(RefusalCodes) >= 2),
    %% Verify all refusal codes are in valid range
    lists:foreach(fun(E) ->
        Code = E#error_code_req.code,
        ?assert(Code >= 1001 andalso Code =< 1089)
    end, RefusalCodes).

%%%====================================================================
%%% 4. Validation Tests (5 tests)
%%%====================================================================

%% @doc Test validate_message/1 validates JSON-RPC structure
validate_message_validates_json_rpc_structure() ->
    %% Valid message
    ValidMsg = #{
        jsonrpc => <<"2.0">>,
        method => <<"initialize">>,
        id => 1
    },
    ?assertEqual({ok, valid_message}, erlmcp_spec_parser:validate_message(ValidMsg)),

    %% Invalid version
    InvalidVersion = #{jsonrpc => <<"1.0">>, method => <<"test">>, id => 1},
    ?assertEqual({error, invalid_jsonrpc_version},
                 erlmcp_spec_parser:validate_message(InvalidVersion)),

    %% Missing fields
    MissingFields = #{method => <<"test">>},
    ?assertEqual({error, missing_required_fields},
                 erlmcp_spec_parser:validate_message(MissingFields)).

%% @doc Test validate_method_call/2 validates parameters
validate_method_call_validates_parameters() ->
    %% Valid tools/call with required params
    ValidParams = #{name => <<"test_tool">>, arguments => #{}},
    ?assertEqual({ok, valid_params},
                 erlmcp_spec_parser:validate_method_call(<<"tools/call">>, ValidParams)),

    %% Missing required parameter
    MissingParam = #{name => <<"test_tool">>},
    ?assertMatch({error, {missing_params, _}},
                 erlmcp_spec_parser:validate_method_call(<<"tools/call">>, MissingParam)),

    %% Unknown method
    ?assertEqual({error, method_not_found},
                 erlmcp_spec_parser:validate_method_call(<<"unknown/method">>, #{})).

%% @doc Test validate_error_code/1 validates error codes
validate_error_code_validates_error_codes() ->
    %% Valid error code
    ?assertEqual({ok, valid_error_code},
                 erlmcp_spec_parser:validate_error_code(-32700)),
    ?assertEqual({ok, valid_error_code},
                 erlmcp_spec_parser:validate_error_code(-32001)),

    %% Unknown error code
    ?assertEqual({error, unknown_error_code},
                 erlmcp_spec_parser:validate_error_code(-99999)).

%% @doc Test check_capability_support/2 checks capabilities
check_capability_support_checks_capabilities() ->
    %% Full support
    ?assertEqual({ok, capability_supported},
                 erlmcp_spec_parser:check_capability_support(
                     <<"resources">>, [<<"subscribe">>, <<"list">>, <<"read">>])),
    ?assertEqual({ok, capability_supported},
                 erlmcp_spec_parser:check_capability_support(
                     <<"tools">>, [<<"list">>, <<"call">>])),

    %% Missing features
    ?assertMatch({error, {missing_features, _}},
                 erlmcp_spec_parser:check_capability_support(
                     <<"resources">>, [<<"list">>])),

    %% Unknown capability
    ?assertEqual({error, unknown_capability},
                 erlmcp_spec_parser:check_capability_support(
                     <<"unknown_cap">>, [])).

%% @doc Test generate_validation_rules/0 returns all rules
generate_validation_rules_returns_all_rules() ->
    {ok, Rules} = erlmcp_spec_parser:generate_validation_rules(),
    ?assert(is_list(Rules)),
    ?assert(length(Rules) >= 4),
    ?assert(is_record(hd(Rules), validation_rule)),
    %% Verify rule structure
    Rule = hd(Rules),
    ?assert(is_binary(Rule#validation_rule.rule_id)),
    ?assert(is_atom(Rule#validation_rule.severity)),
    ?assert(is_atom(Rule#validation_rule.category)).

%%%====================================================================
%%% 5. Transport Requirements Tests (3 tests)
%%%====================================================================

%% @doc Test get_transport_requirements/0 returns all transports
get_transport_requirements_returns_all_transports() ->
    {ok, Transports} = erlmcp_spec_parser:get_transport_requirements(),
    ?assert(is_list(Transports)),
    ?assert(length(Transports) >= 3),
    ?assert(is_record(hd(Transports), transport_req)).

%% @doc Test get_transport_requirements/1 returns specific transport
get_transport_requirements_returns_specific_transport() ->
    {ok, Transport} = erlmcp_spec_parser:get_transport_requirements(<<"stdio">>),
    ?assertMatch(#transport_req{name = <<"stdio">>}, Transport),
    ?assertEqual(stream_based, Transport#transport_req.transport_type),
    ?assertEqual(json_delimiter, Transport#transport_req.framing),
    ?assertEqual(true, Transport#transport_req.connection_oriented),
    ?assertEqual(false, Transport#transport_req.multiplexing_support).

%% @doc Test that STDIO transport has correct requirements
stdio_transport_has_correct_requirements() ->
    {ok, Stdio} = erlmcp_spec_parser:get_transport_requirements(<<"stdio">>),
    RequiredFeatures = Stdio#transport_req.required_features,
    ?assert(lists:member(<<"newline_delimiter">>, RequiredFeatures)),
    ?assertEqual(1, length(RequiredFeatures)).

%%%====================================================================
%%% 6. Capability Requirements Tests (3 tests)
%%%====================================================================

%% @doc Test get_capability_requirements/0 returns all capabilities
get_capability_requirements_returns_all_capabilities() ->
    {ok, Capabilities} = erlmcp_spec_parser:get_capability_requirements(),
    ?assert(is_list(Capabilities)),
    ?assert(length(Capabilities) >= 4),
    ?assert(is_record(hd(Capabilities), capability_req)).

%% @doc Test get_capability_requirements/1 returns specific capability
get_capability_requirements_returns_specific_capability() ->
    {ok, Capability} = erlmcp_spec_parser:get_capability_requirements(<<"resources">>),
    ?assertMatch(#capability_req{name = <<"resources">>}, Capability),
    ?assertEqual(server, Capability#capability_req.category),
    ?assertEqual(false, Capability#capability_req.required),
    ?assertEqual([], Capability#capability_req.dependencies).

%% @doc Test that resources capability has correct features
resources_capability_has_correct_features() ->
    {ok, Resources} = erlmcp_spec_parser:get_capability_requirements(<<"resources">>),
    Features = Resources#capability_req.features,
    ?assert(lists:member(<<"subscribe">>, Features)),
    ?assert(lists:member(<<"list">>, Features)),
    ?assert(lists:member(<<"read">>, Features)),
    ?assertEqual(3, length(Features)).

%%%====================================================================
%%% Additional Edge Case Tests (within fixture)
%%%====================================================================

%% @doc Test list operations
list_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             {ok, Methods} = erlmcp_spec_parser:list_methods(),
             ?assert(is_list(Methods)),
             ?assert(length(Methods) >= 7),
             ?assert(lists:member(<<"initialize">>, Methods)),
             ?assert(lists:member(<<"tools/list">>, Methods))
         end),
         ?_test(begin
             {ok, ErrorCodes} = erlmcp_spec_parser:list_error_codes(),
             ?assert(is_list(ErrorCodes)),
             ?assert(lists:member(-32700, ErrorCodes)),
             ?assert(lists:member(-32001, ErrorCodes))
         end),
         ?_test(begin
             {ok, Transports} = erlmcp_spec_parser:list_transports(),
             ?assert(is_list(Transports)),
             ?assert(lists:member(<<"stdio">>, Transports))
         end),
         ?_test(begin
             {ok, Capabilities} = erlmcp_spec_parser:list_capabilities(),
             ?assert(is_list(Capabilities)),
             ?assert(lists:member(<<"resources">>, Capabilities)),
             ?assert(lists:member(<<"tools">>, Capabilities))
         end)
     ] end}.

%% @doc Test error cases for not found
not_found_errors_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_assertEqual({error, not_found},
                       erlmcp_spec_parser:get_method_requirements(<<"nonexistent">>)),
         ?_assertEqual({error, not_found},
                       erlmcp_spec_parser:get_error_requirements(-99999)),
         ?_assertEqual({error, not_found},
                       erlmcp_spec_parser:get_transport_requirements(<<"nonexistent">>)),
         ?_assertEqual({error, not_found},
                       erlmcp_spec_parser:get_capability_requirements(<<"nonexistent">>))
     ] end}.

%% @doc Test method result specifications
method_result_spec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             {ok, ToolsList} = erlmcp_spec_parser:get_method_requirements(<<"tools/list">>),
             ResultSpec = ToolsList#method_req.result_spec,
             ?assert(is_map(ResultSpec)),
             ?assert(maps:is_key(tools, ResultSpec))
         end),
         ?_test(begin
             {ok, ResourcesList} = erlmcp_spec_parser:get_method_requirements(<<"resources/list">>),
             ResultSpec2 = ResourcesList#method_req.result_spec,
             ?assert(is_map(ResultSpec2)),
             ?assert(maps:is_key(resources, ResultSpec2))
         end)
     ] end}.

%% @doc Test error code retry strategies
error_retry_strategy_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             {ok, ParseError} = erlmcp_spec_parser:get_error_requirements(-32700),
             ?assertEqual(abort, ParseError#error_code_req.retry_strategy)
         end),
         ?_test(begin
             {ok, InternalError} = erlmcp_spec_parser:get_error_requirements(-32603),
             ?assertEqual(retry, InternalError#error_code_req.retry_strategy)
         end),
         ?_test(begin
             {ok, OverloadError} = erlmcp_spec_parser:get_error_requirements(-32010),
             ?assertEqual(retry, OverloadError#error_code_req.retry_strategy)
         end)
     ] end}.

%% @doc Test transport multiplexing support
transport_multiplexing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             {ok, Stdio} = erlmcp_spec_parser:get_transport_requirements(<<"stdio">>),
             ?assertEqual(false, Stdio#transport_req.multiplexing_support)
         end),
         ?_test(begin
             {ok, Sse} = erlmcp_spec_parser:get_transport_requirements(<<"sse">>),
             ?assertEqual(true, Sse#transport_req.multiplexing_support)
         end)
     ] end}.

%% @doc Test capability dependencies
capability_dependencies_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             {ok, Resources} = erlmcp_spec_parser:get_capability_requirements(<<"resources">>),
             ?assertEqual([], Resources#capability_req.dependencies)
         end),
         ?_test(begin
             {ok, Tools} = erlmcp_spec_parser:get_capability_requirements(<<"tools">>),
             ?assertEqual([], Tools#capability_req.dependencies)
         end)
     ] end}.

%% @doc Test method deprecation status
method_deprecation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             {ok, Init} = erlmcp_spec_parser:get_method_requirements(<<"initialize">>),
             ?assertEqual(stable, Init#method_req.deprecation_status)
         end),
         ?_test(begin
             {ok, ToolsCall} = erlmcp_spec_parser:get_method_requirements(<<"tools/call">>),
             ?assertEqual(stable, ToolsCall#method_req.deprecation_status)
         end)
     ] end}.
