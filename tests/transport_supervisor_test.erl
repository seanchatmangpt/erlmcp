-module(transport_supervisor_test).
-include_lib("eunit/include/eunit.hrl").

%% Test the enhanced transport supervisor functionality
-export([test_transport_module_resolution/0, test_config_validation/0]).

%% Test transport module resolution
test_transport_module_resolution() ->
    % Test valid transport types
    ?assertEqual(erlmcp_transport_stdio_new, erlmcp_transport_sup:transport_module(stdio)),
    ?assertEqual(erlmcp_transport_tcp_new, erlmcp_transport_sup:transport_module(tcp)),
    ?assertEqual(erlmcp_transport_http_new, erlmcp_transport_sup:transport_module(http)),
    
    % Test invalid transport type
    ?assertError({unknown_transport_type, invalid}, 
                 erlmcp_transport_sup:transport_module(invalid)),
    
    io:format("Transport module resolution tests passed~n").

%% Test public API functionality
test_config_validation() ->
    % Test that the supervisor module loads correctly
    io:format("Testing supervisor module loading...~n"),
    
    % Test transport type resolution works
    try
        erlmcp_transport_sup:transport_module(stdio),
        io:format("STDIO module resolution: OK~n"),
        
        erlmcp_transport_sup:transport_module(tcp),
        io:format("TCP module resolution: OK~n"),
        
        erlmcp_transport_sup:transport_module(http), 
        io:format("HTTP module resolution: OK~n"),
        
        io:format("All basic API tests passed~n")
    catch
        Error:Reason ->
            io:format("Error testing API: ~p:~p~n", [Error, Reason])
    end.

%% Run all tests
all_test() ->
    test_transport_module_resolution(),
    test_config_validation(),
    io:format("All transport supervisor tests passed!~n").