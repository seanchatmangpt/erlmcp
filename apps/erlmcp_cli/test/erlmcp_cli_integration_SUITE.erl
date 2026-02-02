%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Integration Test Suite (Common Test)
%%%
%%% Integration tests for erlmcp_cli application
%%%
%%% Chicago School TDD:
%%% - Real processes and supervisors
%%% - End-to-end workflow testing
%%% - State-based verification only
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_integration_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Suite Callbacks
%%%====================================================================

%% @doc Test suite setup
init_per_suite(Config) ->
    %% Start application
    {ok, _} = application:ensure_all_started(erlmcp_cli),

    %% Initialize test data
    Config.

%% @doc Test suite cleanup
end_per_suite(Config) ->
    %% Stop application
    ok = application:stop(erlmcp_cli),

    %% Clean up processes
    cleanup_processes(),
    Config.

%% @doc Test group setup
init_per_group(_GroupName, Config) ->
    Config.

%% @doc Test group cleanup
end_per_group(_GroupName, Config) ->
    Config.

%% @doc Test case setup
init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    cleanup_test_state(),
    Config.

%% @doc Test case cleanup
end_per_testcase(TestCase, Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),
    cleanup_test_state(),
    Config.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc End-to-end CLI workflow test
end_to_end_workflow_test(_Config) ->
    %% Initialize registry
    Command = #{
        name => <<"test.e2e">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"E2E test">>,
        category => <<"test">>,
        safety_level => safe
    },
    ok = erlmcp_cli_registry:register_command(Command),

    %% Execute command
    Result = erlmcp_cli_registry:execute_command(<<"test.e2e">>, []),
    ?assert(is_map(Result)),

    %% Verify metrics updated
    Metrics = erlmcp_cli_registry:get_metrics(),
    ?assert(maps:get("executions", Metrics, 0) > 0).

%% @doc Multi-transport integration test
multi_transport_integration_test(_Config) ->
    %% Initialize stdio transport
    StdioConfig = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"test-multi">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, StdioConfig),
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Send message via stdio
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Verify metrics
    {ok, Stats} = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(maps:get(<<"messages_sent">>, Stats, 0) > 0),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%% @doc Session lifecycle integration test
session_lifecycle_integration_test(_Config) ->
    %% Create session
    SessionId = <<"integration-session">>,
    {ok, SessionPid} = erlmcp_cli_session:create_session(SessionId, #{}),
    ?assert(is_process_alive(SessionPid)),

    %% Start session
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Verify state
    {ok, SessionState} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(started, maps:get(status, SessionState)),

    %% Stop session
    ok = erlmcp_cli_session:stop_session(SessionId),

    %% Terminate
    ok = erlmcp_cli_session:terminate_session(SessionId),
    timer:sleep(50),
    ?assertNot(is_process_alive(SessionPid)).

%% @doc JSON-RPC end-to-end flow test
json_rpc_e2e_flow_test(_Config) ->
    %% Create JSON-RPC request
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => null,
        <<"id">> => 1
    },

    RequestJson = jsx:encode(Request),

    %% Handle request
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),

    %% Verify response
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(1, maps:get(<<"id">>, Response)),
    ?assert(is_map(maps:get(<<"result">>, Response))).

%% @doc Registry and session coordination test
registry_session_coordination_test(_Config) ->
    %% Create session
    SessionId = <<"coord-session">>,
    {ok, _SessionPid} = erlmcp_cli_session:create_session(SessionId, #{}),
    ok = erlmcp_cli_session:start_session(SessionId),

    %% Register command
    Command = #{
        name => <<"test.coord">>,
        module => erlmcp_cli_registry,
        function => test_function,
        arity => 1,
        description => <<"Coordination test">>,
        category => <<"test">>,
        safety_level => safe
    },
    ok = erlmcp_cli_registry:register_command(Command),

    %% Execute command in session context
    Result = erlmcp_cli_registry:execute_command(<<"test.coord">>, []),
    ?assert(is_map(Result)),

    %% Verify session still valid
    {ok, SessionState} = erlmcp_cli_session:get_state(SessionId),
    ?assertEqual(started, maps:get(status, SessionState)),

    %% Cleanup
    ok = erlmcp_cli_session:terminate_session(SessionId).

%% @doc Transport failover test
transport_failover_test(_Config) ->
    %% Start stdio transport
    StdioConfig = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"failover-session">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, StdioConfig),

    %% Verify active
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Close transport
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Verify inactive
    ?assertNot(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Restart transport (failover)
    ok = erlmcp_cli_transport:transport(<<"stdio">>, StdioConfig),
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%% @doc Metrics collection integration test
metrics_collection_integration_test(_Config) ->
    %% Reset metrics
    ok = erlmcp_cli_metrics:reset_all_metrics(),

    %% Execute various operations to generate metrics
    ok = erlmcp_cli_metrics:increment_counter(<<"test.counter">>, 5),
    ok = erlmcp_cli_metrics:set_gauge(<<"test.gauge">>, 42),
    lists:foreach(fun(N) ->
        erlmcp_cli_metrics:record_histogram(<<"test.histogram">>, N)
    end, lists:seq(1, 10)),

    %% Export all metrics
    AllMetrics = erlmcp_cli_metrics:export_metrics(),

    %% Verify structure
    ?assert(is_map(AllMetrics)),
    ?assert(is_map(maps:get(<<"counters">>, AllMetrics))),
    ?assert(is_map(maps:get(<<"gauges">>, AllMetrics))),
    ?assert(is_map(maps:get(<<"histograms">>, AllMetrics))).

%% @doc Full application lifecycle test
full_application_lifecycle_test(_Config) ->
    %% Verify application running
    ?assert(is_process_alive(whereis(erlmcp_cli_sup))),

    %% Verify child processes
    ?assert(is_process_alive(whereis(erlmcp_cli_registry))),
    ?assert(is_process_alive(whereis(erlmcp_cli_metrics))),

    %% Perform operations
    ok = erlmcp_cli_metrics:increment_counter(<<"lifecycle.counter">>),
    Result = erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    ?assert(is_map(Result)),

    %% Verify still running
    ?assert(is_process_alive(whereis(erlmcp_cli_sup))).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Clean up any remaining processes
cleanup_processes() ->
    Processes = erlang:processes(),
    lists:foreach(fun(Pid) ->
        case erlang:process_info(Pid) of
            {registered, [Name]} when
                  Name =:= erlmcp_cli_sup;
                  Name =:= erlmcp_cli_registry;
                  Name =:= erlmcp_cli_metrics ->
                erlang:exit(Pid, kill);
            _ -> ok
        end
    end, Processes),
    timer:sleep(100),
    ok.

%% @doc Clean up test-specific state
cleanup_test_state() ->
    %% Stop transports
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),
    ok = erlmcp_cli_transport:close_transport(<<"tcp">>),
    ok = erlmcp_cli_transport:close_transport(<<"http">>),
    ok = erlmcp_cli_transport:close_transport(<<"ws">>),
    ok = erlmcp_cli_transport:close_transport(<<"sse">>),

    %% Reset metrics
    ok = erlmcp_cli_metrics:reset_all_metrics(),

    %% Clear messages
    process_flag(trap_exit, true),
    receive
        _ -> cleanup_test_state()
    after 0 ->
        process_flag(trap_exit, false),
        ok
    end.

%% @doc Test function stub
test_function(_Args) ->
    #{<<"result">> => <<"ok">>}.

%% @doc Wrapper for erlang:whereis
whereis(Name) ->
    case erlang:whereis(Name) of
        undefined -> undefined;
        Pid -> Pid
    end.
