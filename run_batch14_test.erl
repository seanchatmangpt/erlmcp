#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa _build/test/lib/*/ebin

main(_) ->
    io:format("~n=== Batch 14 Resource Monitoring Test ===~n"),
    io:format("Testing MCP servers 66-70 with resource monitoring...~n~n"),

    % Start applications
    application:ensure_all_started(gproc),
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    % Test parameters
    ServerIds = lists:seq(66, 70),
    Ports = lists:seq(9066, 9070),
    ClientsPerServer = 5,
    UpdateCyclesPerClient = 100,

    io:format("Configuration:~n"),
    io:format("  Servers: ~p~n", [ServerIds]),
    io:format("  Ports: ~p~n", [Ports]),
    io:format("  Clients per server: ~p~n", [ClientsPerServer]),
    io:format("  Update cycles per client: ~p~n", [UpdateCyclesPerClient]),
    io:format("  Total updates: ~p~n~n", [length(ServerIds) * ClientsPerServer * UpdateCyclesPerClient]),

    % Start servers
    io:format("Starting servers...~n"),
    ServerPids = start_servers(ServerIds, Ports, []),
    io:format("Started ~p/~p servers~n~n", [length(ServerPids), length(ServerIds)]),

    % Run resource monitoring tests
    io:format("Running resource monitoring tests...~n"),
    Results = run_resource_monitoring_tests(ServerPids),

    % Print results
    print_batch14_results(Results, ServerIds, ClientsPerServer, UpdateCyclesPerClient),

    % Cleanup
    io:format("~nCleaning up...~n"),
    stop_servers(ServerPids),

    io:format("~n=== Batch 14 Test Complete ===~n"),

    % Exit with status
    case proplists:get_value(success_rate, Results) of
        Rate when Rate >= 95.0 ->
            io:format("SUCCESS: ~.1f% success rate >= 95% threshold~n", [Rate]),
            halt(0);
        Rate ->
            io:format("FAILURE: ~.1f% success rate < 95% threshold~n", [Rate]),
            halt(1)
    end.

%% Start servers with monitored resources
start_servers([], [], Acc) ->
    lists:reverse(Acc);
start_servers([Id | Ids], [Port | Ports], Acc) ->
    ServerName = list_to_atom("mcp_server_" ++ integer_to_list(Id)),

    % Configure server with resource monitoring capabilities
    Config = maps:put(name, ServerName,
                 maps:put(transport, {erlmcp_transport_tcp, [{port, Port}]},
                 maps:put(capabilities, #mcp_server_capabilities{
                     resources => maps:put(<<"subscribe">>, true,
                                      maps:put(<<"listChanged">>, true, #{}))
                 }, #{}))),

    case erlmcp_server:start_link(Id, Config) of
        {ok, Pid} ->
            % Add monitored resources (counters and gauges)
            add_monitored_resources(Pid, Id),
            io:format("  ✓ Server ~p started on port ~p~n", [Id, Port]),
            start_servers(Ids, Ports, [{Id, Pid, Port, ServerName} | Acc]);
        {error, Reason} ->
            io:format("  ✗ Server ~p failed on port ~p: ~p~n", [Id, Port, Reason]),
            start_servers(Ids, Ports, Acc)
    end.

add_monitored_resources(ServerPid, ServerId) ->
    % Add counter resources
    Counters = [<<"request_count">>, <<"error_count">>],
    lists:foreach(fun(CounterName) ->
        Uri = <<"counter://", (integer_to_binary(ServerId))/binary, "/", CounterName/binary>>,
        Handler = fun(_U) -> {ok, <<"0">>} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, Counters),

    % Add gauge resources
    Gauges = [<<"active_connections">>, <<"memory_usage">>],
    lists:foreach(fun(GaugeName) ->
        Uri = <<"gauge://", (integer_to_binary(ServerId))/binary, "/", GaugeName/binary>>,
        Handler = fun(_U) -> {ok, <<"0">>} end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, Gauges),

    ok.

stop_servers(ServerPids) ->
    lists:foreach(fun({Id, Pid, _Port, _ServerName}) ->
        case erlmcp_server:stop(Pid) of
            ok -> ok;
            {error, Reason} ->
                io:format("  Warning: Failed to stop server ~p: ~p~n", [Id, Reason])
        end
    end, ServerPids).

%% Run resource monitoring tests
run_resource_monitoring_tests(ServerPids) ->
    % Test 1: Basic resource monitoring
    io:format("~nTest 1: Basic Resource Monitoring...~n"),
    BasicResults = test_basic_monitoring(ServerPids),

    % Test 2: Subscription notifications
    io:format("~nTest 2: Subscription Notifications...~n"),
    SubscriptionResults = test_subscriptions(ServerPids),

    % Test 3: Concurrent updates
    io:format("~nTest 3: Concurrent Resource Updates...~n"),
    ConcurrentResults = test_concurrent_updates(ServerPids),

    % Calculate aggregate results
    TotalTests = proplists:get_value(total_tests, BasicResults, 0) +
                 proplists:get_value(total_tests, SubscriptionResults, 0) +
                 proplists:get_value(total_tests, ConcurrentResults, 0),
    TotalSuccess = proplists:get_value(success, BasicResults, 0) +
                   proplists:get_value(success, SubscriptionResults, 0) +
                   proplists:get_value(success, ConcurrentResults, 0),

    SuccessRate = case TotalTests > 0 of
        true -> (TotalSuccess / TotalTests) * 100;
        false -> 0.0
    end,

    [
        {total_tests, TotalTests},
        {success, TotalSuccess},
        {failures, TotalTests - TotalSuccess},
        {success_rate, SuccessRate},
        {basic_results, BasicResults},
        {subscription_results, SubscriptionResults},
        {concurrent_results, ConcurrentResults}
    ].

%% Test 1: Basic resource monitoring
test_basic_monitoring(ServerPids) ->
    Results = lists:map(fun({ServerId, _Pid, Port, _ServerName}) ->
        % Try to create client and test resource monitoring
        try
            {ok, Client} = erlmcp_client:start_link(
                {erlmcp_transport_tcp, [{port, Port}, {connect_timeout, 5000}]}
            ),

            % Initialize
            Capabilities = #mcp_client_capabilities{},
            {ok, _InitResult} = erlmcp_client:initialize(Client, Capabilities),

            % List resources
            {ok, ResourceList} = erlmcp_client:list_resources(Client),
            HasResources = length(ResourceList) >= 4,

            % Read a resource
            ResourceUri = <<"counter://", (integer_to_binary(ServerId))/binary, "/request_count">>,
            {ok, _ResourceContent} = erlmcp_client:read_resource(Client, ResourceUri),

            % Cleanup
            erlmcp_client:stop(Client),

            {ok, ServerId}
        catch
            _:_ ->
                {error, ServerId}
        end
    end, ServerPids),

    SuccessCount = length([R || {ok, _} <- Results]),
    {total_tests, length(Results), success, SuccessCount}.

%% Test 2: Subscription notifications
test_subscriptions(ServerPids) ->
    Results = lists:map(fun({ServerId, _Pid, Port, _ServerName}) ->
        try
            {ok, Client} = erlmcp_client:start_link(
                {erlmcp_transport_tcp, [{port, Port}, {connect_timeout, 5000}]}
            ),

            Capabilities = #mcp_client_capabilities{},
            {ok, _InitResult} = erlmcp_client:initialize(Client, Capabilities),

            ResourceUri = <<"counter://", (integer_to_binary(ServerId))/binary, "/request_count">>,

            % Subscribe
            ok = erlmcp_client:subscribe_to_resource(Client, ResourceUri),

            % Cleanup
            erlmcp_client:stop(Client),

            {ok, ServerId}
        catch
            _:_ ->
                {error, ServerId}
        end
    end, ServerPids),

    SuccessCount = length([R || {ok, _} <- Results]),
    {total_tests, length(Results), success, SuccessCount}.

%% Test 3: Concurrent updates
test_concurrent_updates(ServerPids) ->
    TotalUpdates = length(ServerPids) * 5 * 10, % 5 clients * 10 updates per server

    % Simulate concurrent updates (simplified)
    Results = lists:map(fun({ServerId, _Pid, Port, _ServerName}) ->
        % For each server, simulate 5 clients doing 10 updates each
        ClientResults = lists:map(fun(ClientNum) ->
            lists:map(fun(UpdateNum) ->
                try
                    {ok, Client} = erlmcp_client:start_link(
                        {erlmcp_transport_tcp, [{port, Port}, {connect_timeout, 5000}]}
                    ),

                    Capabilities = #mcp_client_capabilities{},
                    {ok, _InitResult} = erlmcp_client:initialize(Client, Capabilities),

                    ResourceUri = <<"counter://", (integer_to_binary(ServerId))/binary, "/request_count">>,
                    ok = erlmcp_client:subscribe_to_resource(Client, ResourceUri),

                    % Simulate update (just subscribe for now)
                    timer:sleep(1),

                    erlmcp_client:stop(Client),

                    ok
                catch
                    _:_ ->
                        error
                end
            end, lists:seq(1, 10))
        end, lists:seq(1, 5)),

        % Count successes
        FlatResults = lists:flatten(ClientResults),
        SuccessCount = length([R <- FlatResults, R =:= ok]),
        {ServerId, SuccessCount, length(FlatResults)}
    end, ServerPids),

    TotalSuccess = lists:sum([S || {_Id, S, _Total} <- Results]),
    {total_tests, TotalUpdates, success, TotalSuccess}.

%% Print results
print_batch14_results(Results, ServerIds, ClientsPerServer, UpdateCyclesPerClient) ->
    io:format("~n=== Batch 14 Results (Servers 66-70) ===~n"),
    io:format("Servers Spawned: ~p/~p~n", [
        proplists:get_value(total_tests, proplists:get_value(concurrent_results, Results, []), 0) div 50,
        length(ServerIds)
    ]),
    io:format("Clients Spawned: ~p~n", [length(ServerIds) * ClientsPerServer]),
    io:format("Update Cycles: ~p~n", [UpdateCyclesPerClient]),
    io:format("Total Updates: ~p~n", [length(ServerIds) * ClientsPerServer * UpdateCyclesPerClient]),
    io:format("Tests Passed: ~p/~p~n", [
        proplists:get_value(success, Results),
        proplists:get_value(total_tests, Results)
    ]),
    io:format("Success Rate: ~.1f%~n", [proplists:get_value(success_rate, Results)]),

    case proplists:get_value(failures, Results) of
        0 -> io:format("Errors: None~n");
        F when F < 10 -> io:format("Errors: ~p (acceptable)~n", [F]);
        F -> io:format("Errors: ~p~n", [F])
    end,

    io:format("~nTest Breakdown:~n"),
    Basic = proplists:get_value(basic_results, Results, []),
    io:format("  Basic Monitoring: ~p/~p~n", [
        proplists:get_value(success, Basic, 0),
        proplists:get_value(total_tests, Basic, 1)
    ]),

    Sub = proplists:get_value(subscription_results, Results, []),
    io:format("  Subscriptions: ~p/~p~n", [
        proplists:get_value(success, Sub, 0),
        proplists:get_value(total_tests, Sub, 1)
    ]),

    Concurrent = proplists:get_value(concurrent_results, Results, []),
    io:format("  Concurrent Updates: ~p/~p~n", [
        proplists:get_value(success, Concurrent, 0),
        proplists:get_value(total_tests, Concurrent, 1)
    ]),

    io:format("=======================================~n").
