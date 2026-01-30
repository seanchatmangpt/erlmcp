#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% Connection Leak Fix Verification Script
%%%
%%% This script verifies that connection slots are properly released
%%% on all exit paths from the TCP transport handler.
%%%
%%% Usage: escript test_connection_leak_fix.erl
%%%-------------------------------------------------------------------

-mode(compile).

main(_) ->
    io:format("~n=== Connection Leak Fix Verification ===~n~n"),

    %% Start in embedded mode
    application:load(erlmcp),

    %% Start required applications
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(ranch),

    %% Start connection limiter
    {ok, LimiterPid} = erlmcp_connection_limiter:start_link(),
    ok = erlmcp_connection_limiter:set_limit(100),

    io:format("✓ Connection limiter started~n"),

    %% Run test scenarios
    Tests = [
        {"Normal lifecycle", fun test_normal_lifecycle/0},
        {"Concurrent connections", fun test_concurrent_connections/0},
        {"Slot reuse", fun test_slot_reuse/0}
    ],

    Results = lists:map(fun({Name, TestFun}) ->
        io:format("~nRunning: ~s...~n", [Name]),
        InitialCount = erlmcp_connection_limiter:get_connection_count(),
        Result = try TestFun() of
            ok ->
                FinalCount = erlmcp_connection_limiter:get_connection_count(),
                case FinalCount =:= InitialCount of
                    true ->
                        io:format("  ✓ PASS: Slot count ~p -> ~p (no leak)~n",
                                  [InitialCount, FinalCount]),
                        {Name, pass};
                    false ->
                        io:format("  ✗ FAIL: Slot count ~p -> ~p (LEAK DETECTED!)~n",
                                  [InitialCount, FinalCount]),
                        {Name, fail}
                end
        catch
            Type:Error:Stacktrace ->
                io:format("  ✗ ERROR: ~p:~p~n  Stacktrace: ~p~n",
                          [Type, Error, Stacktrace]),
                {Name, error}
        end,
        %% Give time for cleanup
        timer:sleep(200),
        Result
    end, Tests),

    %% Stop connection limiter
    erlmcp_connection_limiter:stop(),

    %% Print summary
    io:format("~n=== Test Summary ===~n"),
    PassCount = length([R || {_, pass} = R <- Results]),
    FailCount = length([R || {_, fail} = R <- Results]),
    ErrorCount = length([R || {_, error} = R <- Results]),
    Total = length(Results),

    lists:foreach(fun({Name, Status}) ->
        Icon = case Status of
            pass -> "✓";
            fail -> "✗";
            error -> "⚠"
        end,
        io:format("  ~s ~s: ~p~n", [Icon, Name, Status])
    end, Results),

    io:format("~nTotal: ~p/~p passed~n", [PassCount, Total]),

    case FailCount + ErrorCount of
        0 ->
            io:format("~n✓ All tests passed - No connection leaks detected!~n~n"),
            halt(0);
        _ ->
            io:format("~n✗ Some tests failed - Connection leaks may exist!~n~n"),
            halt(1)
    end.

%%%====================================================================
%%% Test Functions
%%%====================================================================

%% @doc Test normal connection lifecycle
test_normal_lifecycle() ->
    %% Start a TCP server
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        port => 0,
        server_id => test_server,
        owner => self()
    }),

    %% Get the actual port
    Port = get_server_port(ServerPid),

    %% Accept a connection
    {ok, ClientSocket} = gen_tcp:connect({127,0,0,1}, Port, []),

    %% Wait for handler to start
    timer:sleep(100),

    %% Close the connection
    gen_tcp:close(ClientSocket),

    %% Stop the server
    catch exit(ServerPid, normal),

    %% Wait for cleanup
    timer:sleep(200),

    ok.

%% @doc Test concurrent connections
test_concurrent_connections() ->
    %% Start a TCP server
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        port => 0,
        server_id => test_server,
        owner => self()
    }),

    %% Get the actual port
    Port = get_server_port(ServerPid),

    %% Create 5 concurrent connections
    NumConns = 5,
    Clients = lists:map(fun(_) ->
        {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, []),
        Socket
    end, lists:seq(1, NumConns)),

    %% Wait for handlers to start
    timer:sleep(200),

    %% Close all connections
    lists:foreach(fun(Socket) ->
        gen_tcp:close(Socket)
    end, Clients),

    %% Stop the server
    catch exit(ServerPid, normal),

    %% Wait for cleanup
    timer:sleep(300),

    ok.

%% @doc Test slot reuse after handler termination
test_slot_reuse() ->
    %% Start a TCP server
    {ok, ServerPid} = erlmcp_transport_tcp:start_server(#{
        port => 0,
        server_id => test_server,
        owner => self()
    }),

    %% Get the actual port
    Port = get_server_port(ServerPid),

    %% Create and close a connection multiple times
    lists:foreach(fun(_Iter) ->
        %% Connect
        {ok, Socket} = gen_tcp:connect({127,0,0,1}, Port, []),
        timer:sleep(50),

        %% Disconnect
        gen_tcp:close(Socket),
        timer:sleep(50)
    end, lists:seq(1, 5)),

    %% Stop the server
    catch exit(ServerPid, normal),

    %% Wait for final cleanup
    timer:sleep(200),

    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Get the actual port of a server (when port was 0)
get_server_port(ServerPid) ->
    try
        {ok, State} = gen_server:call(ServerPid, get_state, 5000),
        State#state.port
    catch
        _:_ ->
            %% Fallback: try to find listening port
            {ok, Ports} = inet:ports(),
            find_listening_port(Ports)
    end.

%% @doc Find a listening TCP port
find_listening_port([]) ->
    0;
find_listening_port([Port | Rest]) ->
    case inet:port(Port) of
        {ok, PortNum} when PortNum > 1024 ->
            PortNum;
        _ ->
            find_listening_port(Rest)
    end.
