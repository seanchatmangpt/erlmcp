%%%-------------------------------------------------------------------
%% @doc Integration Tests for Gap #30: Protocol Version Error Response
%% Tests the complete flow from client request to error response
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_gap30_integration_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite: Integration Tests for Protocol Version Errors
%%====================================================================

protocol_version_integration_test_() ->
    {
        "Gap #30: Integration Tests - Protocol Version Error Response",
        [
            ?_test(test_capabilities_validation()),
            ?_test(test_error_format()),
            ?_test(test_supported_versions_list()),
            ?_test(test_all_unsupported_versions_handled()),
            ?_test(test_error_data_structure()),
            ?_test(test_version_codes())
        ]
    }.

%%====================================================================
%% Test Cases
%%====================================================================

%% Test 1: erlmcp_capabilities validates correctly
test_capabilities_validation() ->
    % Test valid versions
    ?assertEqual(ok, erlmcp_capabilities:validate_protocol_version(<<"2025-11-25">>)),
    ?assertEqual(ok, erlmcp_capabilities:validate_protocol_version(<<"2024-11-05">>)),

    % Test invalid versions return error with data
    InvalidResult = erlmcp_capabilities:validate_protocol_version(<<"invalid">>),
    ?assertMatch({error, _}, InvalidResult).

%% Test 2: Error format matches MCP spec
test_error_format() ->
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"1.0.0">>),

    % Must be a map
    ?assert(is_map(ErrorData)),

    % Must have required fields
    ?assert(maps:is_key(<<"client_version">>, ErrorData)),
    ?assert(maps:is_key(<<"supported_versions">>, ErrorData)),

    % client_version must be the requested version
    ?assertEqual(<<"1.0.0">>, maps:get(<<"client_version">>, ErrorData)),

    % supported_versions must be a non-empty list of binaries
    SupportedVersions = maps:get(<<"supported_versions">>, ErrorData),
    ?assert(is_list(SupportedVersions)),
    ?assert(length(SupportedVersions) > 0),
    lists:foreach(fun(V) -> ?assert(is_binary(V)) end, SupportedVersions).

%% Test 3: Supported versions list is consistent
test_supported_versions_list() ->
    {error, ErrorData1} = erlmcp_capabilities:validate_protocol_version(<<"1.0">>),
    {error, ErrorData2} = erlmcp_capabilities:validate_protocol_version(<<"2.0">>),
    {error, ErrorData3} = erlmcp_capabilities:validate_protocol_version(<<"3.0">>),

    Ver1 = maps:get(<<"supported_versions">>, ErrorData1),
    Ver2 = maps:get(<<"supported_versions">>, ErrorData2),
    Ver3 = maps:get(<<"supported_versions">>, ErrorData3),

    % All should be the same
    ?assertEqual(Ver1, Ver2),
    ?assertEqual(Ver2, Ver3).

%% Test 4: All unsupported versions are handled
test_all_unsupported_versions_handled() ->
    UnsupportedVersions = [
        <<"0.1.0">>,
        <<"1.0.0">>,
        <<"1.5.0">>,
        <<"2.0.0">>,
        <<"2025-01-01">>,
        <<"2025-09-01">>,
        <<"2025-12-01">>,
        <<"invalid">>,
        <<"">>
    ],

    lists:foreach(
        fun(Version) ->
            Result = erlmcp_capabilities:validate_protocol_version(Version),
            ?assertMatch({error, _}, Result),
            {error, ErrorData} = Result,
            ?assert(is_map(ErrorData)),
            ?assert(maps:is_key(<<"client_version">>, ErrorData)),
            ?assert(maps:is_key(<<"supported_versions">>, ErrorData))
        end,
        UnsupportedVersions
    ).

%% Test 5: Error data structure is complete
test_error_data_structure() ->
    % Gap #30: Error data should include:
    % 1. client_version: the version the client requested
    % 2. supported_versions: list of versions server supports
    {error, ErrorData} = erlmcp_capabilities:validate_protocol_version(<<"2.5.0">>),

    % Verify structure
    ClientVersion = maps:get(<<"client_version">>, ErrorData, undefined),
    SupportedVersions = maps:get(<<"supported_versions">>, ErrorData, undefined),

    ?assertEqual(<<"2.5.0">>, ClientVersion),
    ?assertNotEqual(undefined, SupportedVersions),
    ?assert(is_list(SupportedVersions)),

    % Supported versions should include known versions
    ?assert(lists:member(<<"2025-11-25">>, SupportedVersions)),
    ?assert(lists:member(<<"2024-11-05">>, SupportedVersions)).

%% Test 6: Error codes and constants are correct
test_version_codes() ->
    % Verify error code is -32003
    ErrorCode = ?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION,
    ?assertEqual(-32003, ErrorCode),

    % Verify error message is correct
    ErrorMessage = ?MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION,
    ?assertEqual(<<"Unsupported protocol version">>, ErrorMessage).
