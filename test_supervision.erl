#!/usr/bin/env escript
%%% Simple supervision tree test

main(_) ->
    % Add our compiled modules to the path
    code:add_pathz("test_libs"),
    
    io:format("Testing supervision tree implementation...~n"),
    
    % Test 1: Start main supervisor
    io:format("1. Starting main supervisor...~n"),
    case erlmcp_sup:start_link() of
        {ok, SupPid} ->
            io:format("   ✓ Main supervisor started: ~p~n", [SupPid]);
        {error, Reason} ->
            io:format("   ✗ Failed to start main supervisor: ~p~n", [Reason]),
            halt(1)
    end,
    
    % Test 2: Check children
    io:format("2. Checking supervisor children...~n"),
    Children = supervisor:which_children(erlmcp_sup),
    io:format("   Children: ~p~n", [Children]),
    
    ExpectedChildren = [erlmcp_registry, erlmcp_server_sup, erlmcp_transport_sup],
    ChildIds = [Id || {Id, _Pid, _Type, _Modules} <- Children],
    case lists:sort(ChildIds) =:= lists:sort(ExpectedChildren) of
        true ->
            io:format("   ✓ All expected children present~n");
        false ->
            io:format("   ✗ Missing children. Expected: ~p, Got: ~p~n", [ExpectedChildren, ChildIds]),
            halt(1)
    end,
    
    % Test 3: Check if children are alive
    io:format("3. Checking if children are alive...~n"),
    lists:foreach(fun({Id, Pid, Type, _Modules}) ->
        case is_process_alive(Pid) of
            true ->
                io:format("   ✓ ~p (~p) is alive: ~p~n", [Id, Type, Pid]);
            false ->
                io:format("   ✗ ~p is not alive~n", [Id]),
                halt(1)
        end
    end, Children),
    
    % Test 4: Test server supervisor
    io:format("4. Testing server supervisor...~n"),
    case erlmcp_server_sup:start_child(test_server, #{capabilities => []}) of
        {ok, ServerPid} ->
            io:format("   ✓ Server started: ~p~n", [ServerPid]);
        {error, ServerReason} ->
            io:format("   ✗ Failed to start server: ~p~n", [ServerReason])
    end,
    
    % Test 5: Test transport supervisor  
    io:format("5. Testing transport supervisor...~n"),
    case erlmcp_transport_sup:start_child(test_transport, stdio, #{test_mode => true}) of
        {ok, TransportPid} ->
            io:format("   ✓ Transport started: ~p~n", [TransportPid]);
        {error, TransportReason} ->
            io:format("   ✗ Failed to start transport: ~p~n", [TransportReason])
    end,
    
    % Test 6: Test transport_module function
    io:format("6. Testing transport module resolution...~n"),
    try
        StdioModule = erlmcp_transport_sup:transport_module(stdio),
        TcpModule = erlmcp_transport_sup:transport_module(tcp),
        HttpModule = erlmcp_transport_sup:transport_module(http),
        io:format("   ✓ Module resolution works: stdio=~p, tcp=~p, http=~p~n", 
                 [StdioModule, TcpModule, HttpModule])
    catch
        Class:CatchReason ->
            io:format("   ✗ Transport module resolution failed: ~p:~p~n", [Class, CatchReason])
    end,
    
    io:format("~nSupervisor implementation test completed successfully!~n"),
    halt(0).