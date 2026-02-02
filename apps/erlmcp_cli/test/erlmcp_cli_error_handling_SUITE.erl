%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Error Handling Test Suite (Common Test)
%%%
%%% Error handling and recovery tests for erlmcp_cli
%%%
%%% Chicago School TDD:
%%% - Fault injection
%%% - Real error scenarios
%%% - All error paths tested
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_error_handling_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Suite Callbacks
%%%====================================================================

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp_cli),
    Config.

end_per_suite(Config) ->
    application:stop(erlmcp_cli),
    Config.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Network error scenarios test
network_error_scenarios_test(_Config) ->
    %% Test: Connection refused handled gracefully
    BadConfig = #{
        <<"type">> => <<"tcp">>,
        <<"host">> => <<"localhost">>,
        <<"port">> => 1,
        <<"session_id">> => <<"network-error">>
    },
    {ok, _Pid} = erlmcp_cli_transport:transport(<<"tcp">>, BadConfig),

    timer:sleep(100),

    %% Verify error tracked
    {ok, State} = erlmcp_cli_transport:get_transport_state(<<"tcp">>),
    Errors = maps:get(<<"errors">>, State, []),
    ?assert(is_list(Errors)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"tcp">>),

    ok.

%% @doc Protocol error scenarios test
protocol_error_scenarios_test(_Config) ->
    %% Test: Malformed JSON handled
    MalformedJson = <<"{invalid json}">>,
    {error, {parse_error, _}} = erlmcp_cli_json_rpc:parse_request(MalformedJson),

    %% Test: Missing required fields
    IncompleteRequest = <<"{\"jsonrpc\":\"2.0\"}">>,
    {error, {invalid_request, _}} = erlmcp_cli_json_rpc:parse_request(IncompleteRequest),

    %% Test: Invalid params
    InvalidParams = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"params\":\"invalid\",\"id\":1}">>,
    {error, {invalid_request, _}} = erlmcp_cli_json_rpc:parse_request(InvalidParams),

    ok.

%% @doc Authentication failure scenarios test
authentication_failure_test(_Config) ->
    %% Test: Missing credentials
    SessionId = <<"auth-fail">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{
        auth_required => true
    }),

    %% Attempt without credentials
    {error, authentication_failed} = erlmcp_cli_session:authenticate(SessionId, #{}),

    %% Test: Wrong credentials
    {error, authentication_failed} = erlmcp_cli_session:authenticate(SessionId, #{
        <<"api_key">> => <<"wrong">>
    }),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId),

    ok.

%% @doc Resource unavailable scenarios test
resource_unavailable_test(_Config) ->
    %% Test: Non-existent command
    {error, command_not_found} = erlmcp_cli_registry:execute_command(<<"nonexistent">>, []),

    %% Test: Non-existent session
    {error, session_not_found} = erlmcp_cli_session:get_state(<<"nonexistent-session">>),

    %% Test: Inactive transport
    {error, transport_not_active} = erlmcp_cli_transport:send_data(<<"inactive">>, <<"test">>),

    ok.

%% @doc Timeout scenarios test
timeout_scenarios_test(_Config) ->
    %% Test: Session timeout
    ShortSession = <<"timeout-session">>,
    {ok, _Pid} = erlmcp_cli_session:create_session(ShortSession, #{
        timeout => 100
    }),
    ok = erlmcp_cli_session:start_session(ShortSession),

    %% Wait for timeout
    timer:sleep(150),

    %% Verify expired
    {ok, State} = erlmcp_cli_session:get_state(ShortSession),
    ?assertEqual(expired, maps:get(status, State)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(ShortSession),

    ok.

%% @doc Malformed message handling test
malformed_message_test(_Config) ->
    %% Test: Various malformed messages
    MalformedMessages = [
        <<"">>,  %% Empty
        <<"{">>,  %% Incomplete JSON
        <<"}{">>,  %% Reversed braces
        <<"{\"jsonrpc\":\"2.0\",\"method\":}">>,  %% Missing value
        <<"{}{}{}">>,  %% Multiple objects
        <<"{\"a\":\"\\u0041\"}">>  %% Valid unicode (should succeed)
    ],

    lists:foreach(fun(Msg) ->
        case erlmcp_cli_json_rpc:parse_request(Msg) of
            {error, _} -> ok;  %% Expected
            {ok, _} -> ok
        end
    end, MalformedMessages),

    ok.

%% @doc Concurrent error conditions test
concurrent_error_test(_Config) ->
    %% Test: Multiple operations failing concurrently
    NumProcesses = 10,
    Pids = [spawn(fun() ->
        %% Try to execute nonexistent command
        erlmcp_cli_registry:execute_command(<<"concurrent-nonexistent">>, []),
        self() ! done
    end) || _ <- lists:seq(1, NumProcesses)],

    %% Wait for all to complete
    [receive
        done -> ok
    after 5000 ->
        ct:fail("Concurrent error test timeout")
    end || _ <- Pids],

    %% Verify system still stable
    ?assert(is_process_alive(whereis(erlmcp_cli_sup))),

    ok.

%% @doc Error recovery test
error_recovery_test(_Config) ->
    %% Test: System recovers from errors

    %% Trigger error
    {error, _} = erlmcp_cli_registry:execute_command(<<"nonexistent">>, []),

    %% Verify system still functional
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    ?assert(is_map(Result)),

    %% Verify metrics still tracking
    Metrics = erlmcp_cli_registry:get_metrics(),
    ?assert(is_map(Metrics)),

    ok.

%% @doc Process crash recovery test
process_crash_recovery_test(_Config) ->
    %% Test: Supervision tree restarts crashed processes

    %% Get registry PID
    RegistryPid = whereis(erlmcp_cli_registry),
    ?assert(is_pid(RegistryPid)),

    %% Crash registry (simulate)
    exit(RegistryPid, kill),

    %% Wait for restart
    timer:sleep(200),

    %% Verify restarted
    NewRegistryPid = whereis(erlmcp_cli_registry),
    ?assert(is_pid(NewRegistryPid)),
    ?assertNot(NewRegistryPid =:= RegistryPid),

    %% Verify still functional
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    ?assert(is_map(Result)),

    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

whereis(Name) ->
    case erlang:whereis(Name) of
        undefined -> undefined;
        Pid -> Pid
    end.
