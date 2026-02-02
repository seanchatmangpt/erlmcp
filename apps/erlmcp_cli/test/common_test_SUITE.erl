%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_cli application
%%%
%%% This suite provides integration tests for the complete CLI application
%%% using Common Test framework.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(common_test_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Case Definitions
%%====================================================================

%% @doc Test suite setup
init_per_suite(Config) ->
    %% Start the application
    {ok, _} = application:ensure_all_started(erlmcp_cli),

    %% Initialize test data
    Config.

%% @doc Test suite cleanup
end_per_suite(Config) ->
    %% Stop the application
    ok = application:stop(erlmcp_cli),

    %% Clean up any remaining processes
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
    %% Log test case start
    ct:pal("Starting test case: ~p", [TestCase]),

    %% Clean up any previous state
    cleanup_test_state(),

    Config.

%% @doc Test case cleanup
end_per_testcase(TestCase, Config) ->
    %% Log test case completion
    ct:pal("Completed test case: ~p", [TestCase]),

    %% Clean up test-specific state
    cleanup_test_state(),

    Config.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test application startup and basic functionality
app_startup_test(_Config) ->
    %% Verify application is running
    ?assert(is_process_alive(whereis(erlmcp_cli_sup))),

    %% Verify child processes are running
    ?assert(is_process_alive(whereis(erlmcp_cli_registry))),
    ?assert(is_process_alive(whereis(erlmcp_cli_metrics))),

    %% Test basic command lookup
    {ok, _} = erlmcp_cli_registry:lookup_command(<<"mcp.health">>),
    ok.

%% @doc Test command registry functionality
command_registry_test(_Config) ->
    %% Register a test command
    Command = #command_info{
        name = <<"test.integration">>,
        module = test_module,
        function = test_function,
        arity = 1,
        description = <<"Test integration command">>,
        category = <<"test">>,
        safety_level = safe
    },

    ok = erlmcp_cli_registry:register_command(Command),

    %% Verify registration
    {ok, RegisteredCommand} = erlmcp_cli_registry:lookup_command(<<"test.integration">>),
    ?assertEqual(Command, RegisteredCommand),

    %% Test command execution
    Args = [<<"test_arg">>],
    Result = erlmcp_cli_registry:execute_command(<<"test.integration">>, Args),
    ?assert(is_map(Result)),
    ok.

%% @doc Test JSON-RPC 2.0 protocol handling
json_rpc_protocol_test(_Config) ->
    %% Test valid request
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => null,
        <<"id">> => 123
    }),

    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>),

    %% Verify response
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(123, maps:get(<<"id">>, Response)),
    ?assert(is_map(maps:get(<<"result">>, Response))),

    %% Test error response
    InvalidRequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"nonexistent">>,
        <<"params">> => null,
        <<"id">> => 456
    }),

    {ok, ErrorResponse} = erlmcp_cli_json_rpc:handle_json_rpc(InvalidRequestJson, #{}, <<"test-session">>),
    Error = maps:get(<<"error">>, ErrorResponse),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ok.

%% @doc Test transport layer integration
transport_integration_test(_Config) ->
    %% Test stdio transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{
        "host" => "localhost",
        "session_id" => <<"test-session">>
    }),

    %% Verify transport is active
    ?assert(erlmcp_cli_transport:is_active(<<"stdio">>)),

    %% Test message sending
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Test transport metrics
    Stats = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(is_map(Stats)),

    %% Close transport
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Verify transport is inactive
    ?assertNot(erlmcp_cli_transport:is_active(<<"stdio">>)),
    ok.

%% @doc Test metrics collection and reporting
metrics_collection_test(_Config) ->
    %% Execute some commands to generate metrics
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),

    %% Get registry metrics
    RegistryMetrics = erlmcp_cli_registry:get_metrics(),
    ?assert(maps:get("lookups", RegistryMetrics, 0) > 0),
    ?assert(maps:get("executions", RegistryMetrics, 0) > 0),

    %% Get overall metrics
    AllMetrics = erlmcp_cli_metrics:export_metrics(),
    ?assert(is_map(AllMetrics)),

    %% Verify metrics are properly structured
    ?assert(is_map(maps:get(<<"counters">>, AllMetrics))),
    ?assert(is_map(maps:get(<<"gauges">>, AllMetrics))),
    ?assert(is_map(maps:get(<<"histograms">>, AllMetrics))),
    ok.

%% @doc Test concurrent operations and thread safety
concurrent_operations_test(_Config) ->
    %% Spawn multiple processes to test concurrent access
    NumProcesses = 5,
    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            %% Execute multiple commands
            lists:foreach(fun(_) ->
                erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
                timer:sleep(10)
            end, lists:seq(1, 3)),

            %% Transport operations
            ok = erlmcp_cli_transport:transport(<<"stdio">>, #{
                "host" => "localhost",
                "session_id" => <<"concurrent-", (integer_to_binary(I))/binary>>
            }),
            Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":", (integer_to_binary(I))/binary, "}">>,
            ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),
            ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

            %% Notify completion
            self() ! {done, I}
        end)
    end, lists:seq(1, NumProcesses)),

    %% Wait for all processes to complete
    lists:foreach(fun(_) ->
        receive
            {done, _} -> ok
        after 10000 ->
            ct:fail("Concurrent operation timed out")
        end
    end, Pids),

    %% Verify metrics reflect concurrent operations
    RegistryMetrics = erlmcp_cli_registry:get_metrics(),
    ?assert(maps:get("executions", RegistryMetrics, 0) >= NumProcesses * 3),
    ok.

%% @doc Test error handling and recovery
error_handling_test(_Config) ->
    %% Test invalid command execution
    {error, invalid_arguments} = erlmcp_cli_registry:execute_command(<<"mcp.health">>),

    %% Test invalid transport operations
    {error, transport_not_active} = erlmcp_cli_transport:send_data(<<"nonexistent">>, <<"test">>),

    %% Test malformed JSON-RPC
    MalformedJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"mcp.health\",\"params\":null,\"id\":1}">>,
    {error, ParseError} = erlmcp_cli_json_rpc:handle_json_rpc(MalformedJson, #{}, <<"test-session">>),
    ?assert(is_tuple(ParseError)),

    %% Verify application is still running after errors
    ?assert(is_process_alive(whereis(erlmcp_cli_sup))),
    ok.

%% @doc test performance characteristics
performance_test(_Config) ->
    %% Measure command execution performance
    Start = erlang:monotonic_time(millisecond),

    NumCommands = 100,
    lists:foreach(fun(_) ->
        erlmcp_cli_registry:execute_command(<<"mcp.health">>, [])
    end, lists:seq(1, NumCommands)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    ct:pal("Executed ~p commands in ~p ms (~p commands/second)",
           [NumCommands, Duration, (NumCommands * 1000) / Duration]),

    %% Verify performance is acceptable (< 100ms per command)
    ?assert(Duration < NumCommands * 100),

    %% Measure JSON-RPC performance
    JsonRpcStart = erlang:monotonic_time(millisecond),

    lists:foreach(fun(I) ->
        RequestJson = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"mcp.health">>,
            <<"params">> => null,
            <<"id">> => I
        }),
        erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test-session">>)
    end, lists:seq(1, NumCommands)),

    JsonRpcEnd = erlang:monotonic_time(millisecond),
    JsonRpcDuration = JsonRpcEnd - JsonRpcStart,

    ct:pal("Processed ~p JSON-RPC requests in ~p ms (~p requests/second)",
           [NumCommands, JsonRpcDuration, (NumCommands * 1000) / JsonRpcDuration]),

    %% Verify JSON-RPC performance is acceptable
    ?assert(JsonRpcDuration < NumCommands * 100),
    ok.

%% @doc Test application configuration changes
configuration_test(_Config) ->
    %% Test different transport configurations
    TransportConfigs = [
        #{ "type" => "stdio" },
        #{ "type" => "stdio", "session_id" => <<"config-test">> },
        #{ "type" => "stdio", "host" => "localhost" }
    ],

    lists:foreach(fun(Config) ->
        %% Test configuration validation
        case erlmcp_cli_transport:validate_config(Config) of
            true -> ok;
            false -> ct:fail("Valid configuration rejected: ~p", [Config])
        end
    end, TransportConfigs),

    %% Test metrics configuration
    ?assertEqual(true, erlmcp_cli_metrics:metrics_enabled()),

    %% Test configuration changes
    ok = erlmcp_cli_metrics:reset_all_metrics(),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Clean up any remaining processes
cleanup_processes() ->
    %% Kill any remaining processes
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

    %% Wait for processes to terminate
    timer:sleep(100),
    ok.

%% @doc Clean up test-specific state
cleanup_test_state() ->
    %% Stop any running transports
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),
    ok = erlmcp_cli_transport:close_transport(<<"tcp">>),
    ok = erlmcp_cli_transport:close_transport(<<"http">>),
    ok = erlmcp_cli_transport:close_transport(<<"ws">>),
    ok = erlmcp_cli_transport:close_transport(<<"sse">>),

    %% Reset metrics
    ok = erlmcp_cli_metrics:reset_all_metrics(),

    %% Clear any pending messages
    process_flag(trap_exit, true),
    receive
        _ -> cleanup_test_state()
    after 0 ->
        process_flag(trap_exit, false),
        ok
    end.

%% @doc Wrapper for erlang:whereis
whereis(Name) ->
    case erlang:whereis(Name) of
        undefined -> undefined;
        Pid -> Pid
    end.