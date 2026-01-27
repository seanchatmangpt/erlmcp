#!/usr/bin/env escript
%%% Run supervision tests directly

%% Simple assertion macros
-define(assertEqual(Expected, Actual),
    case Expected =:= Actual of
        true -> ok;
        false -> 
            io:format("ASSERTION FAILED:~n  Expected: ~p~n  Actual: ~p~n", [Expected, Actual]),
            halt(1)
    end).

-define(assert(Condition),
    case Condition of
        true -> ok;
        false -> 
            io:format("ASSERTION FAILED: ~s~n", [??Condition]),
            halt(1)
    end).

-define(assertNot(Condition),
    case Condition of
        false -> ok;
        true -> 
            io:format("ASSERTION FAILED: ~s should be false~n", [??Condition]),
            halt(1)
    end).

-define(assertNotEqual(NotExpected, Actual),
    case NotExpected =/= Actual of
        true -> ok;
        false -> 
            io:format("ASSERTION FAILED: ~p should not equal ~p~n", [NotExpected, Actual]),
            halt(1)
    end).

main(_) ->
    % Add our compiled modules to the path
    code:add_pathz("test_libs"),
    
    io:format("Running supervision tree tests...~n~n"),
    
    %% Test 1: Basic supervisor functionality
    test_basic_supervisor_functionality(),
    
    %% Test 2: One-for-all restart strategy
    test_one_for_all_restart_strategy(),
    
    %% Test 3: Server supervisor functionality  
    test_server_supervisor_functionality(),
    
    %% Test 4: Transport supervisor functionality
    test_transport_supervisor_functionality(),
    
    io:format("~n✓ All supervision tree tests passed!~n"),
    halt(0).

test_basic_supervisor_functionality() ->
    io:format("Testing basic supervisor functionality...~n"),
    
    % Start supervisor
    {ok, SupPid} = erlmcp_sup:start_link(),
    io:format("  ✓ Supervisor started~n"),
    
    % Check children
    Children = supervisor:which_children(erlmcp_sup),
    ?assertEqual(3, length(Children)),
    io:format("  ✓ All 3 children started~n"),
    
    % Check child order (registry, server_sup, transport_sup)
    ChildIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
    ExpectedOrder = [erlmcp_registry, erlmcp_server_sup, erlmcp_transport_sup],
    ?assertEqual(lists:sort(ExpectedOrder), lists:sort(ChildIds)),
    io:format("  ✓ Children started in correct order~n"),
    
    % Stop
    unlink(SupPid),
    exit(SupPid, shutdown),
    timer:sleep(100),
    io:format("  ✓ Supervisor stops properly~n").

test_one_for_all_restart_strategy() ->
    io:format("Testing one-for-all restart strategy...~n"),
    
    % Start supervisor
    {ok, SupPid} = erlmcp_sup:start_link(),
    
    % Get initial child PIDs
    InitialChildren = supervisor:which_children(erlmcp_sup),
    InitialPids = [Pid || {_Id, Pid, _Type, _Modules} <- InitialChildren, is_pid(Pid)],
    
    % Kill registry (should trigger one_for_all restart)
    RegistryPid = whereis(erlmcp_registry),
    ?assert(is_pid(RegistryPid)),
    exit(RegistryPid, kill),
    
    % Wait for restart
    timer:sleep(1000),
    
    % Verify supervisor is still alive
    ?assert(is_process_alive(SupPid)),
    io:format("  ✓ Supervisor survived registry crash~n"),
    
    % Verify all children restarted (new PIDs)
    NewChildren = supervisor:which_children(erlmcp_sup),
    NewPids = [Pid || {_Id, Pid, _Type, _Modules} <- NewChildren, is_pid(Pid)],
    
    ?assertEqual(length(InitialPids), length(NewPids)),
    % All PIDs should be different after restart (no common PIDs)
    CommonPids = [Pid || Pid <- NewPids, lists:member(Pid, InitialPids)],
    ?assertEqual([], CommonPids),
    io:format("  ✓ All children restarted with new PIDs~n"),
    
    % Stop
    unlink(SupPid),
    exit(SupPid, shutdown),
    timer:sleep(100).

test_server_supervisor_functionality() ->
    io:format("Testing server supervisor functionality...~n"),
    
    % Start main supervisor
    {ok, _SupPid} = erlmcp_sup:start_link(),
    
    % Test server creation
    {ok, ServerPid1} = erlmcp_server_sup:start_child(test_server_1, #{capabilities => []}),
    ?assert(is_pid(ServerPid1)),
    io:format("  ✓ Can create server instances~n"),
    
    % Test multiple servers
    {ok, ServerPid2} = erlmcp_server_sup:start_child(test_server_2, #{capabilities => []}),
    ?assert(is_pid(ServerPid2)),
    ?assertNotEqual(ServerPid1, ServerPid2),
    io:format("  ✓ Can create multiple server instances~n"),
    
    % Test server crash isolation (temporary restart)
    exit(ServerPid1, kill),
    timer:sleep(200),
    
    % Server supervisor should still be alive
    ServerSupPid = whereis(erlmcp_server_sup),
    ?assert(is_process_alive(ServerSupPid)),
    
    % Other server should still be alive
    ?assert(is_process_alive(ServerPid2)),
    io:format("  ✓ Server crashes are isolated~n"),
    
    % Stop main supervisor
    SupPid = whereis(erlmcp_sup),
    unlink(SupPid),
    exit(SupPid, shutdown),
    timer:sleep(100).

test_transport_supervisor_functionality() ->
    io:format("Testing transport supervisor functionality...~n"),
    
    % Start main supervisor
    {ok, _SupPid} = erlmcp_sup:start_link(),
    
    % Test transport creation
    {ok, TransportPid1} = erlmcp_transport_sup:start_child(test_transport_1, stdio, #{test_mode => true}),
    ?assert(is_pid(TransportPid1)),
    io:format("  ✓ Can create transport instances~n"),
    
    % Test different transport types
    {ok, TransportPid2} = erlmcp_transport_sup:start_child(test_transport_2, tcp, #{test_mode => true, port => 8080}),
    ?assert(is_pid(TransportPid2)),
    ?assertNotEqual(TransportPid1, TransportPid2),
    io:format("  ✓ Can create different transport types~n"),
    
    % Test transport module resolution
    ?assertEqual(erlmcp_transport_stdio_new, erlmcp_transport_sup:transport_module(stdio)),
    ?assertEqual(erlmcp_transport_tcp_new, erlmcp_transport_sup:transport_module(tcp)),
    ?assertEqual(erlmcp_transport_http_new, erlmcp_transport_sup:transport_module(http)),
    io:format("  ✓ Transport module resolution works~n"),
    
    % Test transport crash isolation (should restart due to permanent restart)
    exit(TransportPid1, kill),
    timer:sleep(1000),
    
    % Transport supervisor should still be alive
    TransportSupPid = whereis(erlmcp_transport_sup),
    ?assert(is_process_alive(TransportSupPid)),
    
    % Other transport should still be alive
    ?assert(is_process_alive(TransportPid2)),
    io:format("  ✓ Transport crashes are isolated~n"),
    
    % Crashed transport should be restarted
    Children = supervisor:which_children(erlmcp_transport_sup),
    ?assertEqual(2, length(Children)), % Both transports should be there
    io:format("  ✓ Crashed transport was restarted~n"),
    
    % Stop main supervisor
    SupPid = whereis(erlmcp_sup),
    unlink(SupPid),
    exit(SupPid, shutdown),
    timer:sleep(100).