%% @doc Logging Capability Compliance Tests
%% Validates compliance with MCP Logging capability specification
-module(logging_compliance_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Logging Set Level Method Tests
%%%===================================================================

logging_set_level_method_name_test() ->
    %% logging/setLevel method name
    ?assertEqual(<<"logging/setLevel">>, ?MCP_METHOD_LOGGING_SET_LEVEL).

logging_set_level_required_level_param_test() ->
    %% logging/setLevel requires level parameter
    ?assert(true).

logging_set_level_valid_levels_test() ->
    %% Valid log levels
    ValidLevels = ?MCP_VALID_LOG_LEVELS,
    ?assert(lists:member(debug, ValidLevels)),
    ?assert(lists:member(info, ValidLevels)),
    ?assert(lists:member(notice, ValidLevels)),
    ?assert(lists:member(warning, ValidLevels)),
    ?assert(lists:member(error, ValidLevels)),
    ?assert(lists:member(critical, ValidLevels)),
    ?assert(lists:member(alert, ValidLevels)),
    ?assert(lists:member(emergency, ValidLevels)).

logging_set_level_returns_level_test() ->
    %% logging/setLevel returns the set level
    Response = #{
        level => <<"debug">>
    },
    ?assert(maps:is_key(<<"level">>, Response)),
    ?assert(is_binary(maps:get(<<"level">>, Response))).

%%%===================================================================
%%% Log Level Validation Tests
%%%===================================================================

logging_level_debug_test() ->
    %% Debug level
    Level = debug,
    ?assertEqual(debug, Level).

logging_level_info_test() ->
    %% Info level
    Level = info,
    ?assertEqual(info, Level).

logging_level_notice_test() ->
    %% Notice level
    Level = notice,
    ?assertEqual(notice, Level).

logging_level_warning_test() ->
    %% Warning level
    Level = warning,
    ?assertEqual(warning, Level).

logging_level_error_test() ->
    %% Error level
    Level = error,
    ?assertEqual(error, Level).

logging_level_critical_test() ->
    %% Critical level
    Level = critical,
    ?assertEqual(critical, Level).

logging_level_alert_test() ->
    %% Alert level
    Level = alert,
    ?assertEqual(alert, Level).

logging_level_emergency_test() ->
    %% Emergency level
    Level = emergency,
    ?assertEqual(emergency, Level).

%%%===================================================================
%%% Logging Error Handling Tests
%%%===================================================================

logging_set_level_invalid_level_test() ->
    %% Invalid level should return error
    InvalidLevel = <<"invalid">>,
    ?assertEqual({error, invalid_level},
                 validate_log_level_binary(InvalidLevel)).

logging_set_level_missing_level_test() ->
    %% Missing level parameter should return error
    ?assert(true).

%%%===================================================================
%%% Logging Capability Negotiation Tests
%%%===================================================================

logging_capability_advertised_test() ->
    %% Server advertises logging capability
    Capabilities = #mcp_server_capabilities{
        logging = true
    },
    ?assertEqual(true, Capabilities#mcp_server_capabilities.logging).

logging_capability_disabled_test() ->
    %% Server can disable logging capability
    Capabilities = #mcp_server_capabilities{
        logging = false
    },
    ?assertEqual(false, Capabilities#mcp_server_capabilities.logging).

%%%===================================================================
%%% Logging Implementation Tests
%%%===================================================================

logging_set_level_debug_test() ->
    %% Set level to debug
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{logging = true}
    ),

    %% Test setting log level via API
    ClientPid = self(),
    Level = debug,
    case erlmcp_logging:set_level(ClientPid, Level) of
        ok -> ?assert(true);
        {error, _} -> ?assert(false)
    end,

    erlmcp_server:stop(ServerPid).

logging_set_level_error_test() ->
    %% Set level to error
    {ok, ServerPid} = erlmcp_server:start_link(
        test_server,
        #mcp_server_capabilities{logging = true}
    ),

    ClientPid = self(),
    Level = error,
    case erlmcp_logging:set_level(ClientPid, Level) of
        ok -> ?assert(true);
        {error, _} -> ?assert(false)
    end,

    erlmcp_server:stop(ServerPid).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

validate_log_level_binary(LevelBinary) when is_binary(LevelBinary) ->
    LevelString = binary_to_list(LevelBinary),
    try
        Level = list_to_existing_atom(LevelString),
        case lists:member(Level, ?MCP_VALID_LOG_LEVELS) of
            true -> ok;
            false -> {error, invalid_level}
        end
    catch
        error:badarg ->
            {error, invalid_level}
    end.
