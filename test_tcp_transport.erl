-module(test_tcp_transport).
-export([test/0]).

test() ->
    %% Compile the modules we need
    code:add_path("_build/default/lib/erlmcp/ebin"),
    
    %% Test basic initialization in test mode
    Config = #{
        host => "localhost",
        port => 8888,
        test_mode => true,
        transport_id => test_tcp
    },
    
    io:format("Starting TCP transport test...~n"),
    
    try
        case erlmcp_transport_tcp:start_link(test_tcp, Config) of
            {ok, Pid} ->
                io:format("✓ TCP Transport started successfully~n"),
                
                %% Test get_info
                Info = erlmcp_transport_tcp:get_info(Pid),
                io:format("✓ Transport info: ~p~n", [maps:get(transport_id, Info, undefined)]),
                
                %% Test send (in test mode)
                case erlmcp_transport_tcp:send(Pid, <<"test message">>) of
                    ok -> io:format("✓ Send test passed~n");
                    Error -> io:format("✗ Send test failed: ~p~n", [Error])
                end,
                
                %% Test close
                case erlmcp_transport_tcp:close(Pid) of
                    ok -> io:format("✓ Close test passed~n");
                    CloseError -> io:format("✗ Close test failed: ~p~n", [CloseError])
                end,
                
                io:format("TCP Transport test completed successfully!~n");
                
            {error, Reason} ->
                io:format("✗ TCP Transport failed to start: ~p~n", [Reason])
        end
    catch
        Class:CrashReason:Stack ->
            io:format("✗ TCP Transport test crashed: ~p:~p~n~p~n", [Class, CrashReason, Stack])
    end.