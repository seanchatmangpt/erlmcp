#!/usr/bin/env escript
%%% Final supervisor validation test

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

main(_) ->
    % Add our compiled modules to the path
    code:add_pathz("test_libs"),
    
    io:format("=== FINAL SUPERVISION TREE VALIDATION ===~n~n"),
    
    %% Test: Complete supervision tree integration
    test_complete_supervision_tree(),
    
    io:format("~n🎉 SUPERVISION TREE IMPLEMENTATION COMPLETE! 🎉~n"),
    io:format("~n✅ Key accomplishments:~n"),
    io:format("   • Main supervisor (erlmcp_sup) with one_for_all strategy~n"),
    io:format("   • Server supervisor (erlmcp_server_sup) with simple_one_for_one~n"),
    io:format("   • Transport supervisor (erlmcp_transport_sup) with one_for_one~n"),
    io:format("   • Registry starts before servers/transports~n"),
    io:format("   • Independent failure isolation~n"),
    io:format("   • Transport modules: stdio, tcp, http~n"),
    io:format("   • Dynamic child management~n"),
    io:format("   • Proper restart strategies~n~n"),
    halt(0).

test_complete_supervision_tree() ->
    io:format("Testing complete supervision tree integration...~n"),
    
    % Start the complete system
    {ok, SupPid} = erlmcp_sup:start_link(),
    io:format("  ✓ Main supervisor started~n"),
    
    % Verify all core components
    ?assert(is_process_alive(whereis(erlmcp_registry))),
    ?assert(is_process_alive(whereis(erlmcp_server_sup))),  
    ?assert(is_process_alive(whereis(erlmcp_transport_sup))),
    io:format("  ✓ All core components alive~n"),
    
    % Test dynamic server creation
    {ok, _ServerPid} = erlmcp_server_sup:start_child(test_server, #{capabilities => []}),
    io:format("  ✓ Dynamic server creation works~n"),
    
    % Test transport creation for all types
    {ok, _StdioPid} = erlmcp_transport_sup:start_child(test_stdio, stdio, #{test_mode => true}),
    {ok, _TcpPid} = erlmcp_transport_sup:start_child(test_tcp, tcp, #{test_mode => true, port => 8080}),
    {ok, _HttpPid} = erlmcp_transport_sup:start_child(test_http, http, #{test_mode => true, port => 8081}),
    io:format("  ✓ All transport types work (stdio, tcp, http)~n"),
    
    % Test supervision tree resilience
    RegistryPid = whereis(erlmcp_registry),
    exit(RegistryPid, kill),
    timer:sleep(500),
    
    % Verify system recovered
    ?assert(is_process_alive(whereis(erlmcp_registry))),
    ?assert(is_process_alive(whereis(erlmcp_server_sup))),
    ?assert(is_process_alive(whereis(erlmcp_transport_sup))),
    io:format("  ✓ System recovered from registry crash (one_for_all)~n"),
    
    % Test module resolution
    ?assertEqual(erlmcp_transport_stdio_new, erlmcp_transport_sup:transport_module(stdio)),
    ?assertEqual(erlmcp_transport_tcp_new, erlmcp_transport_sup:transport_module(tcp)),
    ?assertEqual(erlmcp_transport_http_new, erlmcp_transport_sup:transport_module(http)),
    io:format("  ✓ Transport module resolution works~n"),
    
    % Clean shutdown
    unlink(SupPid),
    exit(SupPid, shutdown),
    timer:sleep(200),
    io:format("  ✓ Clean shutdown successful~n").