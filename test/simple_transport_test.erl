-module(simple_transport_test).
-export([run/0]).

run() ->
    io:format("=== TRANSPORT FUNCTIONALITY VALIDATION ===~n~n"),
    
    % Test STDIO transport
    io:format("Testing STDIO Transport:~n"),
    test_stdio(),
    
    % Test TCP transport  
    io:format("~nTesting TCP Transport:~n"),
    test_tcp(),
    
    % Test HTTP transport
    io:format("~nTesting HTTP Transport:~n"),
    test_http(),
    
    % Test Registry
    io:format("~nTesting Registry:~n"),
    test_registry(),
    
    io:format("~n=== VALIDATION COMPLETE ===~n").

test_stdio() ->
    try
        Config = #{test_mode => true},
        case erlmcp_transport_stdio_new:start_link(test_stdio, Config) of
            {ok, Pid} ->
                io:format("  ✓ STDIO startup successful~n"),
                
                % Test send
                case erlmcp_transport_stdio_new:send(Pid, <<"test">>) of
                    ok ->
                        io:format("  ✓ STDIO send works~n");
                    _ ->
                        io:format("  ✗ STDIO send failed~n")
                end,
                
                % Test get_info
                try
                    Info = erlmcp_transport_stdio_new:get_info(Pid),
                    Type = maps:get(type, Info, undefined),
                    io:format("  ✓ STDIO get_info works, type: ~p~n", [Type])
                catch
                    _:_ ->
                        io:format("  ✗ STDIO get_info failed~n")
                end,
                
                gen_server:stop(Pid);
            _ ->
                io:format("  ✗ STDIO startup failed~n")
        end
    catch
        _:_ ->
            io:format("  ✗ STDIO transport not functional~n")
    end.

test_tcp() ->
    try
        Config = #{test_mode => true, host => "127.0.0.1", port => 8080},
        case erlmcp_transport_tcp_new:start_link(test_tcp, Config) of
            {ok, Pid} ->
                io:format("  ✓ TCP startup successful~n"),
                
                % Test send
                case erlmcp_transport_tcp_new:send(Pid, <<"test">>) of
                    ok ->
                        io:format("  ✓ TCP send works~n");
                    _ ->
                        io:format("  ✗ TCP send failed~n")
                end,
                
                % Test get_info
                try
                    Info = erlmcp_transport_tcp_new:get_info(Pid),
                    Type = maps:get(type, Info, undefined),
                    io:format("  ✓ TCP get_info works, type: ~p~n", [Type])
                catch
                    _:_ ->
                        io:format("  ✗ TCP get_info failed~n")
                end,
                
                gen_server:stop(Pid);
            _ ->
                io:format("  ✗ TCP startup failed~n")
        end
    catch
        _:_ ->
            io:format("  ✗ TCP transport not functional~n")
    end.

test_http() ->
    try
        Config = #{test_mode => true, port => 8080, path => "/mcp"},
        case erlmcp_transport_http_new:start_link(test_http, Config) of
            {ok, Pid} ->
                io:format("  ✓ HTTP startup successful~n"),
                
                % Test send
                case erlmcp_transport_http_new:send(Pid, <<"test">>) of
                    ok ->
                        io:format("  ✓ HTTP send works~n");
                    _ ->
                        io:format("  ✗ HTTP send failed~n")
                end,
                
                % Test get_info
                try
                    Info = erlmcp_transport_http_new:get_info(Pid),
                    Type = maps:get(type, Info, undefined),
                    io:format("  ✓ HTTP get_info works, type: ~p~n", [Type])
                catch
                    _:_ ->
                        io:format("  ✗ HTTP get_info failed~n")
                end,
                
                gen_server:stop(Pid);
            _ ->
                io:format("  ✗ HTTP startup failed~n")
        end
    catch
        _:_ ->
            io:format("  ✗ HTTP transport not functional~n")
    end.

test_registry() ->
    try
        case erlmcp_registry:start_link() of
            {ok, Pid} ->
                io:format("  ✓ Registry startup successful~n"),
                
                % Test list functions
                try
                    Transports = erlmcp_registry:list_transports(),
                    Servers = erlmcp_registry:list_servers(),
                    io:format("  ✓ Registry listing works (~p transports, ~p servers)~n", 
                             [length(Transports), length(Servers)])
                catch
                    _:_ ->
                        io:format("  ✗ Registry listing failed~n")
                end,
                
                gen_server:stop(Pid);
            {error, {already_started, _}} ->
                io:format("  ✓ Registry already running~n"),
                try
                    Transports = erlmcp_registry:list_transports(),
                    io:format("  ✓ Registry functional (~p transports registered)~n", [length(Transports)])
                catch
                    _:_ ->
                        io:format("  ✗ Registry not functional~n")
                end;
            _ ->
                io:format("  ✗ Registry startup failed~n")
        end
    catch
        _:_ ->
            io:format("  ✗ Registry not functional~n")
    end.