-module(integration_test).
-export([test_message_routing/0, test_error_scenarios/0]).

test_message_routing() ->
    io:format("=== MESSAGE ROUTING TEST ===~n"),
    
    % Start registry
    case start_registry() of
        ok ->
            io:format("✓ Registry started~n"),
            
            % Start transport
            Config = #{test_mode => true},
            case erlmcp_transport_stdio_new:start_link(routing_transport, Config) of
                {ok, TransportPid} ->
                    io:format("✓ Transport started~n"),
                    
                    % Wait for registration
                    timer:sleep(100),
                    
                    % Check if registered
                    case erlmcp_registry:find_transport(routing_transport) of
                        {ok, {RegPid, RegConfig}} when RegPid =:= TransportPid ->
                            io:format("✓ Transport registered successfully~n"),
                            io:format("  Type: ~p~n", [maps:get(type, RegConfig, unknown)]);
                        {ok, {OtherPid, _}} ->
                            io:format("✗ Transport registered with wrong PID: ~p vs ~p~n", [TransportPid, OtherPid]);
                        {error, not_found} ->
                            io:format("✗ Transport not found in registry~n")
                    end,
                    
                    gen_server:stop(TransportPid);
                Error ->
                    io:format("✗ Failed to start transport: ~p~n", [Error])
            end;
        Error ->
            io:format("✗ Failed to start registry: ~p~n", [Error])
    end.

test_error_scenarios() ->
    io:format("~n=== ERROR SCENARIOS TEST ===~n"),
    
    % Test 1: Invalid config
    try
        case erlmcp_transport_stdio_new:start_link(error_test, invalid) of
            {ok, Pid} ->
                io:format("✗ Should have failed with invalid config~n"),
                gen_server:stop(Pid);
            {error, _} ->
                io:format("✓ Correctly rejected invalid config~n")
        end
    catch
        _:_ ->
            io:format("✓ Exception on invalid config (acceptable)~n")
    end,
    
    % Test 2: Operations on non-existent transport
    try
        case erlmcp_registry:find_transport(nonexistent) of
            {error, not_found} ->
                io:format("✓ Registry correctly reports non-existent transport~n");
            Other ->
                io:format("✗ Unexpected result for non-existent transport: ~p~n", [Other])
        end
    catch
        _:_ ->
            io:format("✗ Registry threw exception for valid operation~n")
    end.

start_registry() ->
    case whereis(erlmcp_registry) of
        undefined ->
            case erlmcp_registry:start_link() of
                {ok, _} -> ok;
                Error -> Error
            end;
        _ ->
            ok
    end.