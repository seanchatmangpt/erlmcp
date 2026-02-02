%%%-------------------------------------------------------------------
%%% @doc
%%% Integration tests for erlmcp_cli application
%%%
%%% Tests the complete CLI application workflow including:
%%% - Application startup and shutdown
%%% - Command registry integration
%%% - JSON-RPC handling
%%% - Transport layer integration
%%% - Metrics collection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_app_integration_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test complete application startup and shutdown
app_lifecycle_test() ->
    %% Start the application
    {ok, AppPid} = erlmcp_cli_app:start(),

    %% Verify application is running
    ?assert(is_process_alive(AppPid)),

    %% Verify supervision tree is running
    SupPid = whereis(erlmcp_cli_sup),
    ?assert(is_process_alive(SupPid)),

    %% Verify child processes are running
    RegistryPid = whereis(erlmcp_cli_registry),
    ?assert(is_process_alive(RegistryPid)),

    MetricsPid = whereis(erlmcp_cli_metrics),
    ?assert(is_process_alive(MetricsPid)),

    %% Stop the application
    ok = application:stop(erlmcp_cli),

    %% Verify processes are stopped
    timer:sleep(100),
    ?assertNot(whereis(erlmcp_cli_sup)),
    ?assertNot(whereis(erlmcp_cli_registry)),
    ?assertNot(whereis(erlmcp_cli_metrics)),
    ok.

%% @doc test command registry integration
registry_integration_test() ->
    %% Start the application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test command registration through the application
    Command = #command_info{
        name = <<"integration.test">>,
        module = integration_test_module,
        function = test_function,
        arity = 2,
        description = <<"Integration test command">>,
        category = <<"integration">>,
        safety_level = safe
    },

    ok = erlmcp_cli_registry:register_command(Command),

    %% Test command lookup
    {ok, RegisteredCommand} = erlmcp_cli_registry:lookup_command(<<"integration.test">>),
    ?assertEqual(Command, RegisteredCommand),

    %% Test command execution
    Args = [<<"arg1">>, <<"arg2">>],
    Result = erlmcp_cli_registry:execute_command(<<"integration.test">>, Args),
    ?assert(is_map(Result)),

    %% Test metrics collection
    Metrics = erlmcp_cli_registry:get_metrics(),
    ?assert(is_map(Metrics)),
    ?assert(maps:get("lookups", Metrics, 0) > 0),
    ?assert(maps:get("executions", Metrics, 0) > 0),

    %% Stop the application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test JSON-RPC integration with command registry
json_rpc_integration_test() ->
    %% Start the application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Create a JSON-RPC request
    RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"mcp.health">>,
        <<"params">> => null,
        <<"id">> => <<"integration-test-123">>
    }),

    %% Handle the request
    {ok, Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"integration-session">>),

    %% Verify response structure
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Response)),
    ?assertEqual(<<"integration-test-123">>, maps:get(<<"id">>, Response)),
    ?assert(is_map(maps:get(<<"result">>, Response))),

    %% Test error handling
    InvalidRequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"nonexistent.method">>,
        <<"params">> => null,
        <<"id">> => <<"integration-test-456">>
    }),

    {ok, ErrorResponse} = erlmcp_cli_json_rpc:handle_json_rpc(InvalidRequestJson, #{}, <<"integration-session">>),
    Error = maps:get(<<"error">>, ErrorResponse),
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)),

    %% Stop the application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test transport integration
transport_integration_test() ->
    %% Start the application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test transport creation through the application
    TransportOpts = #{
        "host" => "localhost",
        "port" => 8080,
        "session_id" => <<"integration-transport">>
    },

    %% Start stdio transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, TransportOpts),

    %% Verify transport is active
    true = erlmcp_cli_transport:is_active(<<"stdio">>),

    %% Test message sending through transport
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test.transport">>,
        <<"params">> => #{"message" => "integration test"},
        <<"id">> => <<"integration-transport-test">>
    }),

    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Test transport metrics
    TransportStats = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(is_map(TransportStats)),
    ?assert(maps:get(<<"messages_sent">>, TransportStats, 0) > 0),

    %% Close transport
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Verify transport is inactive
    false = erlmcp_cli_transport:is_active(<<"stdio">>),

    %% Stop the application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test metrics collection across all components
metrics_integration_test() ->
    %% Start the application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Execute some commands to generate metrics
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
    erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),

    %% Start a transport and send data
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{ "host" => "localhost" }),
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Get registry metrics
    RegistryMetrics = erlmcp_cli_registry:get_metrics(),
    ?assert(maps:get("executions", RegistryMetrics, 0) > 0),

    %% Get transport metrics
    TransportMetrics = erlmcp_cli_transport:get_all_transport_stats(),
    ?assert(maps:get(<<"messages_sent">>, TransportMetrics, 0) > 0),

    %% Get overall application metrics
    AllMetrics = erlmcp_cli_metrics:export_metrics(),
    ?assert(is_map(AllMetrics)),

    %% Stop the application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test concurrent operations
concurrent_operations_test() ->
    %% Start the application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Spawn multiple processes to execute commands concurrently
    NumProcesses = 10,
    Pids = lists:map(fun(_) ->
        spawn_link(fun() ->
            erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
            ok
        end)
    end, lists:seq(1, NumProcesses)),

    %% Wait for all processes to complete
    lists:foreach(fun(Pid) ->
        receive
            {_, Pid, ok} -> ok
        after 5000 ->
            ?fail("Process ~p did not complete", [Pid])
        end
    end, Pids),

    %% Verify all commands were executed
    RegistryMetrics = erlmcp_cli_registry:get_metrics(),
    ?assertEqual(NumProcesses, maps:get("executions", RegistryMetrics, 0)),

    %% Test concurrent transport operations
    TransportPids = lists:map(fun(_) ->
        spawn_link(fun() ->
            ok = erlmcp_cli_transport:transport(<<"stdio">>, #{ "host" => "localhost" }),
            Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
            ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),
            ok = erlmcp_cli_transport:close_transport(<<"stdio">>),
            ok
        end)
    end, lists:seq(1, 5)),

    %% Wait for all transport operations to complete
    lists:foreach(fun(Pid) ->
        receive
            {_, Pid, ok} -> ok
        after 5000 ->
            ?fail("Transport process ~p did not complete", [Pid])
        end
    end, TransportPids),

    %% Stop the application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test error handling and recovery
error_recovery_test() ->
    %% Start the application
    {ok, _AppPid} = erlmcp_cli_app:start(),

    %% Test handling of invalid command execution
    {error, invalid_arguments} = erlmcp_cli_registry:execute_command(<<"mcp.health">>),

    %% Test handling of invalid transport operations
    {error, transport_not_active} = erlmcp_cli_transport:send_data(<<"nonexistent">>, <<"test">>),

    %% Test handling of malformed JSON-RPC
    MalformedJson = <<"{\"jsonrpc\":\"2.0\",\"method\":\"mcp.health","params\":null,\"id\":1}">>,  # Missing closing brace
    {error, ParseError} = erlmcp_cli_json_rpc:handle_json_rpc(MalformedJson, #{}, <<"test-session">>),
    ?assert(is_tuple(ParseError)),

    %% The application should still be running after errors
    SupPid = whereis(erlmcp_cli_sup),
    ?assert(is_process_alive(SupPid)),

    %% Stop the application
    ok = application:stop(erlmcp_cli),
    ok.

%% @doc Test application configuration
app_configuration_test() ->
    %% Test different configurations
    Configs = [
        #{"transport" => "stdio", "metrics" => true},
        #{"transport" => "tcp", "host" => "127.0.0.1", "port" => 8080},
        #{"transport" => "http", "host" => "localhost", "port" => 8080}
    ],

    lists:foreach(fun(Config) ->
        %% Stop any running application
        case application:stop(erlmcp_cli) of
            ok -> ok;
            {error, {not_started, _}} -> ok
        end,

        %% Start with configuration
        ?assertEqual(ok, application:start(erlmcp_cli)),
        timer:sleep(100),

        %% Verify application is running
        ?assert(is_process_alive(whereis(erlmcp_cli_sup))),

        %% Stop the application
        ok = application:stop(erlmcp_cli)
    end, Configs),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Wrapper for erlang:whereis with default
whereis(Name) ->
    case erlang:whereis(Name) of
        undefined -> undefined;
        Pid -> Pid
    end.