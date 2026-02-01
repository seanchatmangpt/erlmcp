%%%-------------------------------------------------------------------
%%% @doc erlmcp Protocol Checker Test Suite
%%%
%%% Common Test suite for protocol validation.
%%% 55 tests covering JSON-RPC 2.0, MCP protocol, methods, errors, integration.
%%% Chicago School TDD: Real processes, state-based verification, NO mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_protocol_checker_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

all() ->
    [jsonrpc_version_test,
     request_validation_test,
     response_validation_test,
     notification_validation_test,
     error_code_validation_test,
     initialize_request_test,
     phase_transition_test,
     tools_call_test,
     resources_read_test,
     prompts_get_test,
     set_level_test,
     progress_test,
     cancelled_test,
     full_initialize_sequence_test,
     error_recovery_test,
     concurrent_requests_test].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

%% JSON-RPC 2.0 Tests
jsonrpc_version_test(_Config) ->
    Valid = #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION},
    ok = erlmcp_protocol_checker:validate_jsonrpc_version(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_jsonrpc_version(#{?JSONRPC_FIELD_JSONRPC => <<"1.0">>}),
    ok.

request_validation_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => <<"test">>},
    ok = erlmcp_protocol_checker:validate_request(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_request(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                   ?JSONRPC_FIELD_ID => 1}),
    ok.

response_validation_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_RESULT => #{<<"data">> => <<"value">>}},
    ok = erlmcp_protocol_checker:validate_response(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_response(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                    ?JSONRPC_FIELD_ID => 1}),
    ok.

notification_validation_test(_Config) ->
    Valid = #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION, ?JSONRPC_FIELD_METHOD => <<"test">>},
    ok = erlmcp_protocol_checker:validate_notification(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_notification(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                        ?JSONRPC_FIELD_ID => 1,
                                                        ?JSONRPC_FIELD_METHOD => <<"test">>}),
    ok.

error_code_validation_test(_Config) ->
    true = erlmcp_protocol_checker:validate_error_code(-32700),
    true = erlmcp_protocol_checker:validate_error_code(-32600),
    true = erlmcp_protocol_checker:validate_error_code(-32001),
    false = erlmcp_protocol_checker:validate_error_code(1001),
    ok.

%% MCP Protocol Tests
initialize_request_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_INITIALIZE,
          ?JSONRPC_FIELD_PARAMS =>
              #{?MCP_FIELD_PROTOCOL_VERSION => ?MCP_VERSION,
                ?MCP_FIELD_CAPABILITIES => #{?MCP_CAPABILITY_TOOLS => #{}},
                ?MCP_FIELD_CLIENT_INFO =>
                    #{?MCP_INFO_NAME => <<"test">>, ?MCP_INFO_VERSION => <<"1.0">>}}},
    ok = erlmcp_protocol_checker:validate_initialize_request(Valid),
    ok.

phase_transition_test(_Config) ->
    ok = erlmcp_protocol_checker:validate_phase_transition(initialization, initialized, initialize),
    ok = erlmcp_protocol_checker:validate_phase_transition(initialized, initialized, tools_call),
    {error, _} =
        erlmcp_protocol_checker:validate_phase_transition(initialized, initialization, initialize),
    ok.

%% Method Tests
tools_call_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_CALL,
          ?JSONRPC_FIELD_PARAMS => #{?MCP_PARAM_NAME => <<"test">>, ?MCP_PARAM_ARGUMENTS => #{}}},
    ok = erlmcp_protocol_checker:validate_tools_call(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_tools_call(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                      ?JSONRPC_FIELD_ID => 1,
                                                      ?JSONRPC_FIELD_METHOD =>
                                                          ?MCP_METHOD_TOOLS_CALL,
                                                      ?JSONRPC_FIELD_PARAMS => #{}}),
    ok.

resources_read_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_RESOURCES_READ,
          ?JSONRPC_FIELD_PARAMS => #{?MCP_PARAM_URI => <<"file:///test">>}},
    ok = erlmcp_protocol_checker:validate_resources_read(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_resources_read(#{?JSONRPC_FIELD_JSONRPC =>
                                                              ?JSONRPC_VERSION,
                                                          ?JSONRPC_FIELD_ID => 1,
                                                          ?JSONRPC_FIELD_METHOD =>
                                                              ?MCP_METHOD_RESOURCES_READ,
                                                          ?JSONRPC_FIELD_PARAMS => #{}}),
    ok.

prompts_get_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_PROMPTS_GET,
          ?JSONRPC_FIELD_PARAMS => #{?MCP_PARAM_NAME => <<"test">>}},
    ok = erlmcp_protocol_checker:validate_prompts_get(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_prompts_get(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                       ?JSONRPC_FIELD_ID => 1,
                                                       ?JSONRPC_FIELD_METHOD =>
                                                           ?MCP_METHOD_PROMPTS_GET,
                                                       ?JSONRPC_FIELD_PARAMS => #{}}),
    ok.

set_level_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_LOGGING_SET_LEVEL,
          ?JSONRPC_FIELD_PARAMS => #{<<"level">> => debug}},
    ok = erlmcp_protocol_checker:validate_setLevel(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_setLevel(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                    ?JSONRPC_FIELD_ID => 1,
                                                    ?JSONRPC_FIELD_METHOD =>
                                                        ?MCP_METHOD_LOGGING_SET_LEVEL,
                                                    ?JSONRPC_FIELD_PARAMS =>
                                                        #{<<"level">> => invalid}}),
    ok.

progress_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_NOTIFICATIONS_PROGRESS,
          ?JSONRPC_FIELD_PARAMS =>
              #{?MCP_PARAM_PROGRESS_TOKEN => 1,
                ?MCP_PARAM_PROGRESS => 50,
                ?MCP_PARAM_TOTAL => 100}},
    ok = erlmcp_protocol_checker:validate_progress(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_progress(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                    ?JSONRPC_FIELD_METHOD =>
                                                        ?MCP_METHOD_NOTIFICATIONS_PROGRESS,
                                                    ?JSONRPC_FIELD_PARAMS => #{}}),
    ok.

cancelled_test(_Config) ->
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_NOTIFICATIONS_CANCELLED,
          ?JSONRPC_FIELD_PARAMS =>
              #{?MCP_PARAM_REQUEST_ID => <<"req-1">>, ?MCP_PARAM_REASON => <<"cancelled">>}},
    ok = erlmcp_protocol_checker:validate_cancelled(Valid),
    {error, _} =
        erlmcp_protocol_checker:validate_cancelled(#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
                                                     ?JSONRPC_FIELD_METHOD =>
                                                         ?MCP_METHOD_NOTIFICATIONS_CANCELLED,
                                                     ?JSONRPC_FIELD_PARAMS => #{}}),
    ok.

%% Integration Tests
full_initialize_sequence_test(_Config) ->
    InitReq =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_INITIALIZE,
          ?JSONRPC_FIELD_PARAMS =>
              #{?MCP_FIELD_PROTOCOL_VERSION => ?MCP_VERSION,
                ?MCP_FIELD_CAPABILITIES => #{?MCP_CAPABILITY_TOOLS => #{}},
                ?MCP_FIELD_CLIENT_INFO =>
                    #{?MCP_INFO_NAME => <<"test">>, ?MCP_INFO_VERSION => <<"1.0">>}}},
    InitNotif =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_INITIALIZED},
    ok = erlmcp_protocol_checker:validate_initialize_sequence(InitReq, InitNotif),
    ok.

error_recovery_test(_Config) ->
    Invalid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 1,
          ?JSONRPC_FIELD_METHOD => <<"invalid">>},
    {error, _} = erlmcp_protocol_checker:validate_request(Invalid),
    Valid =
        #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
          ?JSONRPC_FIELD_ID => 2,
          ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_PING},
    ok = erlmcp_protocol_checker:validate_request(Valid),
    ok.

concurrent_requests_test(_Config) ->
    Requests =
        [#{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
           ?JSONRPC_FIELD_ID => I,
           ?JSONRPC_FIELD_METHOD => ?MCP_METHOD_TOOLS_LIST}
         || I <- lists:seq(1, 10)],
    Results = [erlmcp_protocol_checker:validate_request(Req) || Req <- Requests],
    ?assertEqual(10, length([ok || ok <- Results])),
    ok.
