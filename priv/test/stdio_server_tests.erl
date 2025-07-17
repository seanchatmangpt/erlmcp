-module(stdio_server_tests).

-include_lib("eunit/include/eunit.hrl").

stdio_server_debug_test_() ->
    {setup,
     fun setup_debug/0,
     fun cleanup_debug/1,
     fun(_) ->
         [
             ?_test(test_server_startup()),
             ?_test(test_process_registration()),
             ?_test(test_stdin_reader_simulation())
         ]
     end}.

setup_debug() ->
    application:ensure_all_started(erlmcp),
    erlmcp_stdio:stop(),  % Ensure clean start
    ok.

cleanup_debug(_) ->
    erlmcp_stdio:stop().

test_server_startup() ->
    ?debugMsg("Testing server startup"),

    % Start the server
    ?assertEqual(ok, erlmcp_stdio:start()),

    % Check if server is running
    ?assertEqual(true, erlmcp_stdio:is_running()),

    % Check if the process is registered
    Pid = whereis(erlmcp_stdio_server),
    ?assertNotEqual(undefined, Pid),
    ?assert(is_process_alive(Pid)),

    ?debugFmt("Server process: ~p", [Pid]).

test_process_registration() ->
    ?debugMsg("Testing process registration"),

    % Ensure server is running
    case erlmcp_stdio:is_running() of
        false -> erlmcp_stdio:start();
        true -> ok
    end,

    % Check process info
    Pid = whereis(erlmcp_stdio_server),
    ?assertNotEqual(undefined, Pid),

    ProcessInfo = process_info(Pid),
    ?debugFmt("Process info: ~p", [ProcessInfo]),

    % Check if process is trapped exits (should be true)
    TrapExit = process_info(Pid, trap_exit),
    ?debugFmt("Trap exit: ~p", [TrapExit]),

    % Check message queue
    MessageQueue = process_info(Pid, message_queue_len),
    ?debugFmt("Message queue length: ~p", [MessageQueue]).

test_stdin_reader_simulation() ->
    ?debugMsg("Testing stdin reader simulation"),

    % Ensure server is running
    case erlmcp_stdio:is_running() of
        false -> erlmcp_stdio:start();
        true -> ok
    end,

    % Get the server process
    ServerPid = whereis(erlmcp_stdio_server),
    ?assertNotEqual(undefined, ServerPid),

    % Simulate sending a message directly to the server process
    InitMessage = "{\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"1.0.0\"}},\"jsonrpc\":\"2.0\",\"id\":42}",

    ?debugFmt("Sending test message to server: ~s", [InitMessage]),

    % Send the message directly to the server process
    ServerPid ! {stdin_line, InitMessage},

    % Give it a moment to process
    timer:sleep(100),

    % Check if the server processed the message
    % (This is a bit tricky to test without capturing stdout)
    ?assert(is_process_alive(ServerPid)),

    ?debugMsg("Message sent to server process").
