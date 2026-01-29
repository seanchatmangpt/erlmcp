#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin

%%% @doc Quick test of connection monitor functionality

main(_) ->
    io:format("Starting connection monitor test...~n"),

    %% Start the monitor
    {ok, Pid} = erlmcp_connection_monitor:start_link(),
    io:format("Connection monitor started: ~p~n", [Pid]),

    %% Create mock connection
    ConnPid = spawn(fun() ->
        receive
            stop -> ok
        after
            10000 -> timeout
        end
    end),

    %% Monitor connection
    ConnectionInfo = #{
        socket => undefined,
        server_id => test_server,
        transport_id => tcp_transport,
        bytes_sent => 0,
        bytes_received => 0
    },
    ok = erlmcp_connection_monitor:monitor_connection(ConnPid, ConnectionInfo),
    io:format("Monitored connection: ~p~n", [ConnPid]),

    %% Check count
    Count = erlmcp_connection_monitor:get_connection_count(),
    io:format("Connection count: ~p~n", [Count]),

    %% Get stats
    Stats = erlmcp_connection_monitor:get_connection_stats(),
    io:format("Connection stats: ~p~n", [Stats]),

    %% Unmonitor
    ok = erlmcp_connection_monitor:unmonitor_connection(ConnPid),
    io:format("Unmonitored connection~n"),

    %% Check count again
    Count2 = erlmcp_connection_monitor:get_connection_count(),
    io:format("Connection count after unmonitor: ~p~n", [Count2]),

    %% Stop the monitor
    erlmcp_connection_monitor:stop(),
    io:format("Connection monitor stopped~n"),

    %% Kill mock connection
    exit(ConnPid, kill),

    io:format("Test completed successfully!~n"),
    init:stop(0).
