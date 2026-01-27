%%%-------------------------------------------------------------------
%% @doc Test Suite for Gap #30: Protocol Version Error with Supported Versions
%% Tests that unsupported protocol versions return error code -32003
%% with supported versions in the error data field (MCP 2025-11-25)
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_gap30_protocol_version_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    application:stop(erlmcp),
    ok.

%%====================================================================
%% Test Suite: Protocol Version Validation
%%====================================================================

%% Gap #30: Main test group for protocol version error handling
protocol_version_error_test_() ->
    {
        "Gap #30: Protocol Version Error with Supported Versions",
        {
            setup,
            fun setup/0,
            fun cleanup/1,
            [
                ?_test(test_supported_version_2025_11_25()),
                ?_test(test_supported_version_2024_11_05()),
                ?_test(test_unsupported_version_returns_error()),
                ?_test(test_unsupported_version_error_code()),
                ?_test(test_unsupported_version_error_message()),
                ?_test(test_unsupported_version_error_data_field()),
                ?_test(test_error_data_includes_client_version()),
                ?_test(test_error_data_includes_supported_versions()),
                ?_test(test_supported_versions_list_not_empty()),
                ?_test(test_supported_versions_include_both_versions()),
                ?_test(test_version_validation_function()),
                ?_test(test_format_error_function()),
                ?_test(test_error_data_structure_format()),
                ?_test(test_multiple_unsupported_versions()),
                ?_test(test_error_response_structure_complete())
            ]
        }
    }.

%%====================================================================
%% Test Cases: Supported Versions
%%====================================================================

%% Gap #30 Test 1: Verify 2025-11-25 is supported
test_supported_version_2025_11_25() ->
    Result = erlmcp_capabilities:validate_protocol_version(<<"2025-11-25">>),
    ?assertEqual(ok, Result).

%% Gap #30 Test 2: Verify 2024-11-05 is supported
test_supported_version_2024_11_05() ->
    Result = erlmcp_capabilities:validate_protocol_version(<<"2024-11-05">>),
    ?assertEqual(ok, Result).

%%====================================================================
%% Test Cases: Unsupported Versions
%%====================================================================

%% Gap #30 Test 3: Unsupported version returns error
test_unsupported_version_returns_error() ->
    Result = erlmcp_capabilities:validate_protocol_version(<<"1.0.0">>),
    ?assertMatch({error, _}, Result).

%% Gap #30 Test 4: Error code is -32003 (UNSUPPORTED_PROTOCOL_VERSION)
test_unsupported_version_error_code() ->
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"1.0.0">>),
    ?assert(is_map(ErrorData)),
    ?assertEqual(?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION, -32003).

%% Gap #30 Test 5: Verify error message format
test_unsupported_version_error_message() ->
    SupMsg = ?MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION,
    ?assertEqual(<<"Unsupported protocol version">>, SupMsg).

%% Gap #30 Test 6: Error response includes data field
test_unsupported_version_error_data_field() ->
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"2.0.0">>),
    ?assert(is_map(ErrorData)),
    ?assert(maps:is_key(<<"client_version">>, ErrorData)),
    ?assert(maps:is_key(<<"supported_versions">>, ErrorData)).

%%====================================================================
%% Test Cases: Error Data Structure
%%====================================================================

%% Gap #30 Test 7: Error data includes client version
test_error_data_includes_client_version() ->
    ClientVersion = <<"2.0.0">>,
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(ClientVersion),
    ReturnedVersion = maps:get(<<"client_version">>, ErrorData),
    ?assertEqual(ClientVersion, ReturnedVersion).

%% Gap #30 Test 8: Error data includes supported versions list
test_error_data_includes_supported_versions() ->
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"3.0.0">>),
    SupportedVersions = maps:get(<<"supported_versions">>, ErrorData),
    ?assert(is_list(SupportedVersions)),
    ?assert(length(SupportedVersions) > 0).

%% Gap #30 Test 9: Supported versions list is not empty
test_supported_versions_list_not_empty() ->
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"999.999.999">>),
    SupportedVersions = maps:get(<<"supported_versions">>, ErrorData),
    ?assert(length(SupportedVersions) >= 2).

%% Gap #30 Test 10: Supported versions include both documented versions
test_supported_versions_include_both_versions() ->
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"1.0">>),
    SupportedVersions = maps:get(<<"supported_versions">>, ErrorData),
    ?assert(lists:member(<<"2025-11-25">>, SupportedVersions)),
    ?assert(lists:member(<<"2024-11-05">>, SupportedVersions)).

%%====================================================================
%% Test Cases: Validation Functions
%%====================================================================

%% Gap #30 Test 11: validate_protocol_version function works correctly
test_version_validation_function() ->
    % Valid versions
    ?assertEqual(ok, erlmcp_capabilities:validate_protocol_version(<<"2025-11-25">>)),
    ?assertEqual(ok, erlmcp_capabilities:validate_protocol_version(<<"2024-11-05">>)),
    % Invalid versions return error tuple
    InvalidResult = erlmcp_capabilities:validate_protocol_version(<<"invalid">>),
    ?assertMatch({error, _}, InvalidResult).

%% Gap #30 Test 12: format_unsupported_version_error formats correctly
test_format_error_function() ->
    ClientVersion = <<"1.5.0">>,
    SupportedVersions = [<<"2025-11-25">>, <<"2024-11-05">>],
    ErrorData = erlmcp_capabilities:format_unsupported_version_error(ClientVersion, SupportedVersions),
    ?assert(is_map(ErrorData)),
    ?assertEqual(ClientVersion, maps:get(<<"client_version">>, ErrorData)),
    ?assertEqual(SupportedVersions, maps:get(<<"supported_versions">>, ErrorData)).

%% Gap #30 Test 13: Error data structure follows MCP spec format
test_error_data_structure_format() ->
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"2.1.0">>),
    % Check structure matches MCP 2025-11-25 spec
    ?assert(maps:is_key(<<"client_version">>, ErrorData)),
    ?assert(maps:is_key(<<"supported_versions">>, ErrorData)),
    ClientVer = maps:get(<<"client_version">>, ErrorData),
    SupportedVers = maps:get(<<"supported_versions">>, ErrorData),
    ?assert(is_binary(ClientVer)),
    ?assert(is_list(SupportedVers)),
    % All items in supported versions list must be binaries
    lists:foreach(fun(V) -> ?assert(is_binary(V)) end, SupportedVers).

%%====================================================================
%% Test Cases: Edge Cases
%%====================================================================

%% Gap #30 Test 14: Multiple unsupported versions all return same supported list
test_multiple_unsupported_versions() ->
    Versions = [<<"1.0">>, <<"2.0">>, <<"3.0">>, <<"999.999">>],
    ErrorDataList = lists:map(
        fun(V) ->
            {error, E} = erlmcp_capabilities:validate_protocol_version(V),
            E
        end,
        Versions
    ),
    % All should have the same supported versions
    SupportedVersionsList = lists:map(
        fun(E) -> maps:get(<<"supported_versions">>, E) end,
        ErrorDataList
    ),
    % All error responses should have identical supported versions
    [FirstSupported | RestSupported] = SupportedVersionsList,
    lists:foreach(
        fun(S) -> ?assertEqual(FirstSupported, S) end,
        RestSupported
    ).

%% Gap #30 Test 15: Complete error response structure
test_error_response_structure_complete() ->
    ClientVersion = <<"2025-01-01">>,
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(ClientVersion),
    % Verify complete structure
    ErrorCode = -32003,
    ErrorMessage = <<"Unsupported protocol version">>,
    % These should be used by erlmcp_server to build full error response
    ?assertEqual(ClientVersion, maps:get(<<"client_version">>, ErrorData)),
    SupportedVersions = maps:get(<<"supported_versions">>, ErrorData),
    ?assert(is_list(SupportedVersions)),
    ?assert(length(SupportedVersions) > 0),
    % Verify this matches spec requirements
    ?assertEqual(?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION, ErrorCode),
    ?assertEqual(?MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION, ErrorMessage).

