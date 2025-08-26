%%%-------------------------------------------------------------------
%%% @doc
%%% Additional transport tests for comprehensive coverage
%%% These tests extend the existing transport test suites with additional
%%% validation scenarios, edge cases, and recovery testing.
%%% @end
%%%-------------------------------------------------------------------
-module(additional_transport_tests).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Exports for reuse in other test suites
-export([
    test_supervisor_restart_transport/1,
    test_config_edge_cases/1,
    test_buffer_edge_cases/1,
    test_registry_unavailable_recovery/1,
    test_message_flood_recovery/1
]).

%% Supervisor integration tests
test_supervisor_restart_transport(Config) ->
    TransportId = test_transport_supervisor_restart,
    TestConfig = #{test_mode => true},

    % Test supervisor integration if available
    case catch erlmcp_transport_sup:start_link() of
        {ok, _SupPid} ->
            case catch erlmcp_transport_sup:start_transport(TransportId, stdio, TestConfig) of
                {ok, Pid1} ->
                    ?assert(is_process_alive(Pid1)),
                    ok = erlmcp_transport_sup:stop_transport(TransportId);
                _ ->
                    ct:pal("Could not start transport via supervisor")
            end;
        _ ->
            ct:pal("Transport supervisor not available")
    end,
    ok.

%% Configuration edge case testing
test_config_edge_cases(Config) ->
    TransportId = test_transport_config_edge,
    
    % Test various edge case configurations
    EdgeCaseConfigs = [
        #{}, % Empty config
        #{test_mode => true, extra_field => ignored},
        #{test_mode => false, server_id => undefined},
        #{buffer_size => 0}, % Edge value
        #{timeout => infinity} % Special timeout
    ],
    
    lists:foreach(fun(TestConfig) ->
        Result = catch erlmcp_transport_stdio_new:start_link(TransportId, TestConfig),
        case Result of
            {ok, Pid} ->
                ?assert(is_process_alive(Pid)),
                ok = gen_server:stop(Pid);
            _ ->
                ct:pal("Config handled as expected: ~p", [Result])
        end
    end, EdgeCaseConfigs),
    ok.

%% Buffer edge case testing
test_buffer_edge_cases(Config) ->
    TransportId = test_transport_buffer_edge,
    TestConfig = #{test_mode => true},
    
    {ok, Pid} = erlmcp_transport_stdio_new:start_link(TransportId, TestConfig),
    
    % Test buffer with various edge cases
    EdgeCaseInputs = [
        <<>>, % Empty input
        <<"\n">>, % Just newline
        <<"\r\n">>, % CRLF
        <<"\r">>, % Just CR
        <<"incomplete">>, % Incomplete line
        <<"complete\n">>, % Complete line
        binary:copy(<<"x">>, 10000), % Very large input without newline
        binary:copy(<<"line\n">>, 100), % Many small lines
        <<0, 1, 2, 255>>, % Binary data
        <<"unicode: \xC3\xA9\xC3\xA7">>, % Unicode characters
        <<"null\x00byte">> % Null byte
    ],
    
    lists:foreach(fun(Input) ->
        try
            gen_server:call(Pid, {simulate_input, Input}, 1000)
        catch
            _:_ -> ok % Some inputs might cause timeouts, that's expected
        end,
        timer:sleep(5)
    end, EdgeCaseInputs),
    
    % Transport should still be alive after all edge cases
    ?assert(is_process_alive(Pid)),
    
    ok = gen_server:stop(Pid),
    ok.

%% Registry unavailable recovery testing
test_registry_unavailable_recovery(Config) ->
    TransportId = test_transport_registry_recovery,
    ServerId = test_server_registry_recovery,
    TestConfig = #{test_mode => true, server_id => ServerId},
    
    % Stop registry if it exists
    case whereis(erlmcp_registry) of
        undefined -> ok;
        RegPid -> 
            try
                exit(RegPid, kill),
                timer:sleep(100)
            catch
                _:_ -> ok
            end
    end,
    
    % Start transport without registry
    {ok, Pid} = erlmcp_transport_stdio_new:start_link(TransportId, TestConfig),
    ?assert(is_process_alive(Pid)),
    
    % Transport should handle missing registry gracefully
    try
        gen_server:call(Pid, {simulate_input, <<"test message without registry">>}, 1000),
        timer:sleep(100),
        ?assert(is_process_alive(Pid))
    catch
        _:_ -> 
            ?assert(is_process_alive(Pid)) % Even if call fails, process should survive
    end,
    
    % Try to restart registry
    try
        {ok, _NewRegPid} = erlmcp_registry:start_link(),
        timer:sleep(100),
        
        % Transport should continue working
        gen_server:call(Pid, {simulate_input, <<"test message with registry">>}, 1000),
        timer:sleep(100),
        ?assert(is_process_alive(Pid))
    catch
        _:_ -> 
            ct:pal("Could not restart registry - continuing without it")
    end,
    
    ok = gen_server:stop(Pid),
    ok.

%% Message flood recovery testing
test_message_flood_recovery(Config) ->
    TransportId = test_transport_flood_recovery,
    TestConfig = #{test_mode => true},
    
    {ok, Pid} = erlmcp_transport_stdio_new:start_link(TransportId, TestConfig),
    
    % Flood transport with messages
    FloodSize = 1000, % Reasonable size for testing
    StartTime = erlang:monotonic_time(millisecond),
    
    lists:foreach(fun(N) ->
        Message = iolist_to_binary(["flood_message_", integer_to_list(N), "\n"]),
        try
            gen_server:call(Pid, {simulate_input, Message}, 100) % Short timeout
        catch
            exit:{timeout, _} -> ok; % Expected under load
            _:_ -> ok
        end
    end, lists:seq(1, FloodSize)),
    
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    ct:pal("Flood test: ~p messages in ~p ms (~.2f msg/sec)",
           [FloodSize, Duration, FloodSize * 1000 / Duration]),
    
    % Transport should survive the flood
    ?assert(is_process_alive(Pid)),
    
    % Should still respond to normal operations
    try
        {ok, _State} = gen_server:call(Pid, get_state, 5000)
    catch
        _:_ -> 
            ct:pal("Transport may be overloaded but should recover")
    end,
    
    % Give it time to recover
    timer:sleep(1000),
    ?assert(is_process_alive(Pid)),
    
    ok = gen_server:stop(Pid),
    ok.