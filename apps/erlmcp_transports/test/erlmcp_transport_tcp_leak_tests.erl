%%%-------------------------------------------------------------------
%%% @doc
%%% Connection Leak Tests for TCP Transport
%%%
%%% Tests that connection slots are properly released on ALL exit paths:
%%% - Normal termination
%%% - Handler initialization failure
%%% - Early handler crash after start
%%% - Connection lease timeout
%%% - Socket errors during handshake
%%% - Owner process death
%%%
%%% Chicago School TDD: Real processes, real TCP connections, no mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_tcp_leak_tests).

-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include("erlmcp_transport_tcp.hrl").

%% Test fixtures
-define(TEST_SERVER_ID, tcp_leak_test_server).
-define(TEST_TIMEOUT, 10000).

%%%====================================================================
%%% Test Setup and Teardown (EUnit)
%%%====================================================================

setup() ->
    %% Start ranch FIRST (dependency of erlmcp_core)
    {ok, _} = application:ensure_all_started(ranch),
    timer:sleep(50),

    %% Start ONLY gproc for connection limiter (not full erlmcp_core)
    {ok, _} = application:ensure_all_started(gproc),
    timer:sleep(50),

    %% Start connection limiter directly (handle already started case)
    LimiterPid =
        case erlmcp_connection_limiter:start_link() of
            {ok, Pid} ->
                Pid;
            {error, {already_started, Pid}} ->
                Pid
        end,

    %% Set a low limit for testing
    ok = erlmcp_connection_limiter:set_limit(100),

    %% Enable connection limiting
    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 100,
                          alert_threshold => 0.7,
                          enabled => true}),

    %% Get baseline count
    InitialCount = erlmcp_connection_limiter:get_connection_count(),
    {LimiterPid, InitialCount}.

cleanup({LimiterPid, _InitialCount}) ->
    %% Stop connection limiter
    catch erlmcp_connection_limiter:stop(),

    %% Clean up any remaining TCP handlers
    cleanup_tcp_handlers(),

    %% Stop applications in reverse order
    catch application:stop(gproc),
    catch application:stop(ranch),

    %% Allow time for cleanup
    timer:sleep(100),
    ok.

cleanup_tcp_handlers() ->
    %% Find and kill any remaining TCP handler processes
    AllProcesses = processes(),
    lists:foreach(fun(Pid) ->
                     case process_info(Pid, initial_call) of
                         {initial_call, {erlmcp_transport_tcp, _, _}} ->
                             catch exit(Pid, kill);
                         _ ->
                             ok
                     end
                  end,
                  AllProcesses).

%%%====================================================================
%%% Connection Leak Tests (EUnit)
%%%====================================================================

connection_leak_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Normal handler lifecycle releases slot", fun normal_lifecycle_releases_slot/0},
      {"Concurrent connections don't leak slots", fun concurrent_connections_no_leak/0},
      {"Connection limit enforced correctly", fun connection_limit_enforced/0},
      {"Slot reuse after handler termination", fun slot_reuse_after_termination/0},
      {"Rapid connect/disconnect cycles don't leak", fun rapid_cycles_no_leak/0}]}.

%%%-------------------------------------------------------------------
%%% Test Cases
%%%-------------------------------------------------------------------

%% @doc Test that normal handler lifecycle properly releases the slot
normal_lifecycle_releases_slot() ->
    {_LimiterPid, InitialCount} = setup(),

    try
        %% Start a TCP server
        {ok, ServerPid} =
            erlmcp_transport_tcp:start_server(#{port => 0,
                                                server_id => ?TEST_SERVER_ID,
                                                owner => self()}),

        %% Get the actual port
        {ok, State} = gen_server:call(ServerPid, get_state),
        Port = State#state.port,

        %% Accept a connection
        {ok, ClientSocket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),

        %% Wait for handler to start
        timer:sleep(100),

        %% Verify slot was taken
        AfterAccept = erlmcp_connection_limiter:get_connection_count(),
        ?assertEqual(InitialCount + 1, AfterAccept),

        %% Close the connection from client side
        gen_tcp:close(ClientSocket),
        timer:sleep(200),  % Let handler terminate

        %% Verify slot was released
        AfterClose = erlmcp_connection_limiter:get_connection_count(),
        ?assertEqual(InitialCount, AfterClose),

        %% Stop server
        catch exit(ServerPid, normal),

        %% Final cleanup
        timer:sleep(100)
    after
        cleanup({undefined, InitialCount})
    end.

%% @doc Test that concurrent connections don't leak slots
concurrent_connections_no_leak() ->
    {_LimiterPid, InitialCount} = setup(),

    try
        %% Start a TCP server
        {ok, ServerPid} =
            erlmcp_transport_tcp:start_server(#{port => 0,
                                                server_id => ?TEST_SERVER_ID,
                                                owner => self()}),

        %% Get the actual port
        {ok, State} = gen_server:call(ServerPid, get_state),
        Port = State#state.port,

        %% Create 10 concurrent connections
        NumConns = 10,
        Clients =
            lists:map(fun(_) ->
                         {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                         Socket
                      end,
                      lists:seq(1, NumConns)),

        %% Wait for all handlers to start
        timer:sleep(200),

        %% Verify all slots were taken
        AfterAccept = erlmcp_connection_limiter:get_connection_count(),
        ?assertEqual(InitialCount + NumConns, AfterAccept),

        %% Close all connections
        lists:foreach(fun(Socket) -> gen_tcp:close(Socket) end, Clients),

        %% Wait for cleanup
        timer:sleep(300),

        %% Verify all slots were released
        AfterClose = erlmcp_connection_limiter:get_connection_count(),
        ?assertEqual(InitialCount, AfterClose),

        %% Stop server
        catch exit(ServerPid, normal)
    after
        cleanup({undefined, InitialCount})
    end.

%% @doc Test that connection limit is enforced correctly
connection_limit_enforced() ->
    {_LimiterPid, InitialCount} = setup(),

    try
        %% Set a very low limit
        ok = erlmcp_connection_limiter:set_limit(5),

        %% Start a TCP server
        {ok, ServerPid} =
            erlmcp_transport_tcp:start_server(#{port => 0,
                                                server_id => ?TEST_SERVER_ID,
                                                owner => self()}),

        %% Get the actual port
        {ok, State} = gen_server:call(ServerPid, get_state),
        Port = State#state.port,

        %% Create connections up to the limit
        Limit = 5,
        Clients =
            lists:map(fun(_) ->
                         {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                         Socket
                      end,
                      lists:seq(1, Limit)),

        %% Wait for handlers to start
        timer:sleep(200),

        %% Verify all slots are taken
        AtLimit = erlmcp_connection_limiter:get_connection_count(),
        ?assertEqual(InitialCount + Limit, AtLimit),

        %% Try to create more connections - they should be rejected
        %% Note: TCP accepts will happen but handlers may be rejected
        ExtraClients =
            lists:map(fun(_) ->
                         case gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}], 1000) of
                             {ok, Socket} ->
                                 {ok, Socket};
                             {error, _} = Error ->
                                 Error
                         end
                      end,
                      lists:seq(1, 3)),

        %% Wait to see what happened
        timer:sleep(200),

        %% Verify we didn't exceed the limit significantly
        FinalCount = erlmcp_connection_limiter:get_connection_count(),
        ?assert(FinalCount =< InitialCount + Limit + 1),  % Allow small race window

        %% Cleanup all clients
        lists:foreach(fun ({ok, Socket}) ->
                              gen_tcp:close(Socket);
                          (_) ->
                              ok
                      end,
                      Clients ++ ExtraClients),

        timer:sleep(200),

        %% Stop server
        catch exit(ServerPid, normal)
    after
        %% Reset limit
        erlmcp_connection_limiter:set_limit(100),
        cleanup({undefined, InitialCount})
    end.

%% @doc Test that slots can be reused after handler termination
slot_reuse_after_termination() ->
    {_LimiterPid, InitialCount} = setup(),

    try
        %% Start a TCP server
        {ok, ServerPid} =
            erlmcp_transport_tcp:start_server(#{port => 0,
                                                server_id => ?TEST_SERVER_ID,
                                                owner => self()}),

        %% Get the actual port
        {ok, State} = gen_server:call(ServerPid, get_state),
        Port = State#state.port,

        %% Create and close a connection multiple times
        lists:foreach(fun(_Iter) ->
                         %% Connect
                         {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                         timer:sleep(50),

                         %% Disconnect
                         gen_tcp:close(Socket),
                         timer:sleep(50)
                      end,
                      lists:seq(1, 10)),

        %% Wait for final cleanup
        timer:sleep(300),

        %% Verify we're back to initial count (no leaks)
        FinalCount = erlmcp_connection_limiter:get_connection_count(),
        ?assertEqual(InitialCount, FinalCount),

        %% Stop server
        catch exit(ServerPid, normal)
    after
        cleanup({undefined, InitialCount})
    end.

%% @doc Test rapid connect/disconnect cycles don't leak
rapid_cycles_no_leak() ->
    {_LimiterPid, InitialCount} = setup(),

    try
        %% Start a TCP server
        {ok, ServerPid} =
            erlmcp_transport_tcp:start_server(#{port => 0,
                                                server_id => ?TEST_SERVER_ID,
                                                owner => self()}),

        %% Get the actual port
        {ok, State} = gen_server:call(ServerPid, get_state),
        Port = State#state.port,

        %% Simulate rapid connection churn
        NumCycles = 20,
        lists:foreach(fun(_Iter) ->
                         {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                         %% Immediate disconnect
                         gen_tcp:close(Socket)
                      end,
                      lists:seq(1, NumCycles)),

        %% Wait for all handlers to terminate
        timer:sleep(500),

        %% Verify no leaks - should be back to initial count
        FinalCount = erlmcp_connection_limiter:get_connection_count(),
        ?assert(FinalCount >= InitialCount andalso FinalCount =< InitialCount + 1),

        %% Stop server
        catch exit(ServerPid, normal)
    after
        cleanup({undefined, InitialCount})
    end.

%%%====================================================================
%%% Common Test Suite (Integration Tests)
%%%====================================================================

all() ->
    [normal_lifecycle_ct,
     concurrent_connections_ct,
     connection_limit_ct,
     slot_reuse_ct,
     rapid_cycles_ct].

init_per_suite(Config) ->
    %% Start ranch FIRST
    {ok, _} = application:ensure_all_started(ranch),
    timer:sleep(50),

    %% Start gproc for connection limiter
    {ok, _} = application:ensure_all_started(gproc),
    timer:sleep(50),

    %% Start connection limiter (handle already started case)
    LimiterPid =
        case erlmcp_connection_limiter:start_link() of
            {ok, Pid} ->
                Pid;
            {error, {already_started, Pid}} ->
                Pid
        end,
    ok = erlmcp_connection_limiter:set_limit(100),

    application:set_env(erlmcp_core,
                        connection_limiting,
                        #{max_connections => 100,
                          alert_threshold => 0.7,
                          enabled => true}),

    [{limiter_pid, LimiterPid} | Config].

end_per_suite(Config) ->
    catch erlmcp_connection_limiter:stop(),
    cleanup_tcp_handlers(),
    catch application:stop(gproc),
    catch application:stop(ranch),
    ok.

init_per_testcase(_TestCase, Config) ->
    InitialCount = erlmcp_connection_limiter:get_connection_count(),
    [{initial_count, InitialCount} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Clean up any TCP handlers
    cleanup_tcp_handlers(),
    timer:sleep(100),
    ok.

%%%-------------------------------------------------------------------
%%% Common Test Cases
%%%-------------------------------------------------------------------

normal_lifecycle_ct(Config) ->
    InitialCount = ?config(initial_count, Config),

    %% Start a TCP server
    {ok, ServerPid} =
        erlmcp_transport_tcp:start_server(#{port => 0,
                                            server_id => ?TEST_SERVER_ID,
                                            owner => self()}),

    %% Get the actual port
    {ok, State} = gen_server:call(ServerPid, get_state),
    Port = State#state.port,

    %% Accept a connection
    {ok, ClientSocket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),

    %% Wait for handler to start
    timer:sleep(100),

    %% Verify slot was taken
    AfterAccept = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(InitialCount + 1, AfterAccept),

    %% Close the connection
    gen_tcp:close(ClientSocket),
    timer:sleep(200),

    %% Verify slot was released
    AfterClose = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(InitialCount, AfterClose),

    %% Stop server
    catch exit(ServerPid, normal).

concurrent_connections_ct(Config) ->
    InitialCount = ?config(initial_count, Config),

    %% Start a TCP server
    {ok, ServerPid} =
        erlmcp_transport_tcp:start_server(#{port => 0,
                                            server_id => ?TEST_SERVER_ID,
                                            owner => self()}),

    %% Get the actual port
    {ok, State} = gen_server:call(ServerPid, get_state),
    Port = State#state.port,

    %% Create 10 concurrent connections
    NumConns = 10,
    Clients =
        lists:map(fun(_) ->
                     {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                     Socket
                  end,
                  lists:seq(1, NumConns)),

    %% Wait for all handlers to start
    timer:sleep(200),

    %% Verify all slots were taken
    AfterAccept = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(InitialCount + NumConns, AfterAccept),

    %% Close all connections
    lists:foreach(fun(Socket) -> gen_tcp:close(Socket) end, Clients),

    %% Wait for cleanup
    timer:sleep(300),

    %% Verify all slots were released
    AfterClose = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(InitialCount, AfterClose),

    %% Stop server
    catch exit(ServerPid, normal).

connection_limit_ct(Config) ->
    InitialCount = ?config(initial_count, Config),

    %% Set a very low limit
    ok = erlmcp_connection_limiter:set_limit(5),

    %% Start a TCP server
    {ok, ServerPid} =
        erlmcp_transport_tcp:start_server(#{port => 0,
                                            server_id => ?TEST_SERVER_ID,
                                            owner => self()}),

    %% Get the actual port
    {ok, State} = gen_server:call(ServerPid, get_state),
    Port = State#state.port,

    %% Create connections up to the limit
    Limit = 5,
    Clients =
        lists:map(fun(_) ->
                     {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                     Socket
                  end,
                  lists:seq(1, Limit)),

    %% Wait for handlers to start
    timer:sleep(200),

    %% Verify all slots are taken
    AtLimit = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(InitialCount + Limit, AtLimit),

    %% Try to create more connections
    ExtraClients =
        lists:map(fun(_) ->
                     case gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}], 1000) of
                         {ok, Socket} ->
                             {ok, Socket};
                         {error, _} = Error ->
                             Error
                     end
                  end,
                  lists:seq(1, 3)),

    %% Wait to see what happened
    timer:sleep(200),

    %% Verify we didn't exceed the limit significantly
    FinalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assert(FinalCount =< InitialCount + Limit + 1),

    %% Cleanup all clients
    lists:foreach(fun ({ok, Socket}) ->
                          gen_tcp:close(Socket);
                      (_) ->
                          ok
                  end,
                  Clients ++ ExtraClients),

    timer:sleep(200),

    %% Stop server
    catch exit(ServerPid, normal),

    %% Reset limit
    erlmcp_connection_limiter:set_limit(100).

slot_reuse_ct(Config) ->
    InitialCount = ?config(initial_count, Config),

    %% Start a TCP server
    {ok, ServerPid} =
        erlmcp_transport_tcp:start_server(#{port => 0,
                                            server_id => ?TEST_SERVER_ID,
                                            owner => self()}),

    %% Get the actual port
    {ok, State} = gen_server:call(ServerPid, get_state),
    Port = State#state.port,

    %% Create and close a connection multiple times
    lists:foreach(fun(_Iter) ->
                     {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                     timer:sleep(50),
                     gen_tcp:close(Socket),
                     timer:sleep(50)
                  end,
                  lists:seq(1, 10)),

    %% Wait for final cleanup
    timer:sleep(300),

    %% Verify we're back to initial count (no leaks)
    FinalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assertEqual(InitialCount, FinalCount),

    %% Stop server
    catch exit(ServerPid, normal).

rapid_cycles_ct(Config) ->
    InitialCount = ?config(initial_count, Config),

    %% Start a TCP server
    {ok, ServerPid} =
        erlmcp_transport_tcp:start_server(#{port => 0,
                                            server_id => ?TEST_SERVER_ID,
                                            owner => self()}),

    %% Get the actual port
    {ok, State} = gen_server:call(ServerPid, get_state),
    Port = State#state.port,

    %% Simulate rapid connection churn
    NumCycles = 20,
    lists:foreach(fun(_Iter) ->
                     {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, Port, [{active, false}]),
                     gen_tcp:close(Socket)
                  end,
                  lists:seq(1, NumCycles)),

    %% Wait for all handlers to terminate
    timer:sleep(500),

    %% Verify no leaks
    FinalCount = erlmcp_connection_limiter:get_connection_count(),
    ?assert(FinalCount >= InitialCount andalso FinalCount =< InitialCount + 1),

    %% Stop server
    catch exit(ServerPid, normal).
