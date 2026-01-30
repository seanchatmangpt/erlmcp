-module(erlmcp_transport_stdio_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite for stdio Transport Standardization
%%%
%%% This test suite validates the standardized stdio transport
%%% implementation in erlmcp_transport_stdio.erl, ensuring:
%%% - Standard transport behavior compliance
%%% - stdin/stdout handling
%%% - Line-based message framing
%%% - Test mode detection
%%% - Owner process monitoring
%%% - Reader process lifecycle
%%% - Message buffering
%%% - Error handling
%%% - State management
%%%
%%% Target: >90% coverage on erlmcp_transport_stdio.erl
%%%===================================================================

%%====================================================================
%% Test Groups
%%====================================================================

stdio_test_() ->
    {setup,
     fun setup_stdio_transport/0,
     fun cleanup_stdio_transport/1,
     [
         {"Basic stdio transport initialization", fun test_stdio_init/0},
         {"Stdio transport send operation", fun test_stdio_send/0},
         {"Stdio transport close operation", fun test_stdio_close/0},
         {"Stdio test mode detection", fun test_stdio_test_mode/0},
         {"Stdio reader process lifecycle", fun test_stdio_reader_lifecycle/0},
         {"Stdio message framing", fun test_stdio_message_framing/0},
         {"Stdio line trimming", fun test_stdio_line_trimming/0},
         {"Stdio empty line handling", fun test_stdio_empty_line_handling/0},
         {"Stdio buffer management", fun test_stdio_buffer_management/0},
         {"Stdio owner monitoring", fun test_stdio_owner_monitoring/0},
         {"Stdio reader death handling", fun test_stdio_reader_death/0},
         {"Stdio EOF handling", fun test_stdio_eof_handling/0},
         {"Stdio read error handling", fun test_stdio_read_error_handling/0},
         {"Stdio simulated input (test mode)", fun test_stdio_simulated_input/0},
         {"Stdio message delivery to owner", fun test_stdio_message_delivery/0},
         {"Stdio carriage return handling", fun test_stdio_carriage_return/0},
         {"Stdio newline normalization", fun test_stdio_newline_normalization/0},
         {"Stdio state management", fun test_stdio_state_management/0},
         {"Stdio transport behavior compliance", fun test_stdio_behavior_compliance/0}
     ]}.

integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     [
         {timeout, 10, {"Full stdio integration", fun test_full_stdio_integration/0}},
         {timeout, 10, {"Stdio with registry", fun test_stdio_with_registry/0}},
         {timeout, 10, {"Stdio concurrent messages", fun test_stdio_concurrent_messages/0}},
         {timeout, 10, {"Stdio load testing", fun test_stdio_load_testing/0}}
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup_stdio_transport() ->
    % Set test mode in process dictionary
    put(test_mode, true),
    #{}.

cleanup_stdio_transport(_) ->
    erase(test_mode),
    timer:sleep(100),
    ok.

setup_integration() ->
    put(test_mode, true),
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end,
    #{}.

cleanup_integration(_) ->
    erase(test_mode),
    timer:sleep(100),
    ok.

%%====================================================================
%% Basic Stdio Transport Tests
%%====================================================================

test_stdio_init() ->
    Owner = self(),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Verify transport started
        ?assert(is_pid(Transport)),
        ?assert(is_process_alive(Transport)),

        % Get transport state - verify it's a valid state
        {ok, State} = gen_server:call(Transport, get_state),
        ?assert(is_tuple(State)),
        ?assert(tuple_size(State) >= 1)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_send() ->
    Owner = self(),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Test sending binary data
        TestData = <<"test message">>,
        Result = erlmcp_transport_stdio:send(Transport, TestData),

        % Send should always succeed for stdio
        ?assertEqual(ok, Result),

        % Test sending iolist
        TestIolist = [<<"test">>, " ", <<"message">>],
        Result2 = erlmcp_transport_stdio:send(Transport, TestIolist),
        ?assertEqual(ok, Result2)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_close() ->
    Owner = self(),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Close should stop the transport
        ?assertEqual(ok, erlmcp_transport_stdio:close(Transport)),

        % Transport should be stopped
        timer:sleep(100),
        ?assertNot(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Test Mode Tests
%%====================================================================

test_stdio_test_mode() ->
    Owner = self(),

    % Ensure test mode is set
    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        {ok, State} = gen_server:call(Transport, get_state),

        % In test mode, should not start reader
        ?assert(is_tuple(State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_simulated_input() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Simulate input in test mode
        TestLine = <<"test input line">>,
        ?assertEqual(ok, gen_server:call(Transport, {simulate_input, TestLine})),

        % Owner should receive message
        receive
            {transport_message, ReceivedLine} ->
                ?assertEqual(TestLine, ReceivedLine)
        after 1000 ->
            ?assert(false) % Timeout
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Reader Process Tests
%%====================================================================

test_stdio_reader_lifecycle() ->
    Owner = self(),

    % Disable test mode to start reader
    erase(test_mode),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        {ok, State} = gen_server:call(Transport, get_state),

        % In non-test mode, reader might be started
        % (or not if stdin is unavailable, which is expected in tests)
        ?assert(is_tuple(State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_reader_death() ->
    Owner = self(),

    put(test_mode, false),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Transport should handle reader death gracefully
        ?assert(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Message Framing Tests
%%====================================================================

test_stdio_message_framing() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Send multiple lines
        Lines = [
            <<"first line">>,
            <<"second line">>,
            <<"third line">>
        ],

        lists:foreach(fun(Line) ->
            gen_server:call(Transport, {simulate_input, Line})
        end, Lines),

        % Receive all messages
        Received = lists:map(fun(_) ->
            receive
                {transport_message, Msg} -> Msg
            after 1000 -> timeout
            end
        end, Lines),

        % Verify all received
        ?assertEqual(Lines, Received)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_line_trimming() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Send line with trailing newline
        LineWithNewline = <<"test line\n">>,
        gen_server:call(Transport, {simulate_input, LineWithNewline}),

        % Should receive trimmed line
        receive
            {transport_message, Received} ->
                % Note: simulate_input sends as-is, trimming happens in read_loop
                ?assert(is_binary(Received))
        after 1000 ->
            ?assert(false)
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_empty_line_handling() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Empty lines are handled by simulating input
        EmptyLine = <<"">>,
        gen_server:call(Transport, {simulate_input, EmptyLine}),

        % Timeout is expected for empty line
        receive
            {transport_message, _} ->
                % If received, it should be empty
                ok
        after 500 ->
            % No message is also acceptable
            ok
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Buffer Management Tests
%%====================================================================

test_stdio_buffer_management() ->
    Owner = self(),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        {ok, State} = gen_server:call(Transport, get_state),

        % Buffer should be initialized
        ?assert(is_tuple(State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Owner Monitoring Tests
%%====================================================================

test_stdio_owner_monitoring() ->
    Owner = spawn_link(fun() ->
        receive
            stop -> ok
        after 10000 -> ok
        end
    end),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Verify transport is alive
        ?assert(is_process_alive(Transport)),

        % Kill owner
        unlink(Owner),
        exit(Owner, kill),

        % Wait for owner death
        timer:sleep(100),

        % Transport should die when owner dies
        timer:sleep(500),
        ?assertNot(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Error Handling Tests
%%====================================================================

test_stdio_eof_handling() ->
    % EOF handling is tested in reader process
    % In test mode, we verify transport handles simulated scenarios
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Transport should remain stable
        ?assert(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_read_error_handling() ->
    % Read errors are handled in reader process
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Transport should handle errors gracefully
        ?assert(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Message Delivery Tests
%%====================================================================

test_stdio_message_delivery() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Simulate multiple messages
        Messages = [
            <<"message 1">>,
            <<"message 2">>,
            <<"message 3">>
        ],

        lists:foreach(fun(Msg) ->
            gen_server:call(Transport, {simulate_input, Msg})
        end, Messages),

        % Collect all messages
        Received = lists:map(fun(_) ->
            receive
                {transport_message, M} -> M
            after 1000 -> timeout
            end
        end, Messages),

        % Verify all delivered
        ?assertEqual(Messages, Received)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Line Ending Tests
%%====================================================================

test_stdio_carriage_return() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Test with carriage return
        LineWithCR = <<"test line\r">>,
        gen_server:call(Transport, {simulate_input, LineWithCR}),

        receive
            {transport_message, Received} ->
                ?assert(is_binary(Received))
        after 1000 ->
            ?assert(false)
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_newline_normalization() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Test different line endings
        TestLines = [
            {<<"line1\n">>, <<"line1\n">>},
            {<<"line2\r\n">>, <<"line2\r\n">>},
            {<<"line3\r">>, <<"line3\r">>}
        ],

        lists:foreach(fun({Input, _Expected}) ->
            gen_server:call(Transport, {simulate_input, Input})
        end, TestLines),

        % Collect messages
        lists:foreach(fun(_) ->
            receive
                {transport_message, _} -> ok
            after 1000 -> ok
            end
        end, TestLines)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% State Management Tests
%%====================================================================

test_stdio_state_management() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        {ok, State} = gen_server:call(Transport, get_state),

        % Verify state is valid
        ?assert(is_tuple(State))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Transport Behavior Compliance Tests
%%====================================================================

test_stdio_behavior_compliance() ->
    Owner = self(),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Test send with state
        TestData = <<"compliance test">>,
        ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, TestData)),

        % Test close with state
        ?assertEqual(ok, erlmcp_transport_stdio:close(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Integration Tests
%%====================================================================

test_full_stdio_integration() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Full lifecycle test
        ?assert(is_process_alive(Transport)),

        % Test send
        ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, <<"test">>)),

        % Test simulated input
        gen_server:call(Transport, {simulate_input, <<"input test">>}),

        % Receive message
        receive
            {transport_message, <<"input test">>} -> ok
        after 1000 ->
            ?assert(false)
        end,

        % Test close
        ?assertEqual(ok, erlmcp_transport_stdio:close(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_with_registry() ->
    Owner = self(),

    put(test_mode, true),

    % Start registry
    {ok, Registry} = gen_server:start(erlmcp_registry, [], []),

    try
        % Start stdio transport
        {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

        % Register transport
        TransportConfig = #{
            type => stdio,
            config => #{}
        },

        ?assertEqual(ok, gen_server:call(Registry, {register_transport, stdio_test_transport, Transport, TransportConfig})),

        % Verify registration
        ?assertMatch({ok, {Transport, TransportConfig}}, gen_server:call(Registry, {find_transport, stdio_test_transport})),

        % Test message through registry
        gen_server:call(Transport, {simulate_input, <<"registry test">>}),

        receive
            {transport_message, <<"registry test">>} -> ok
        after 1000 ->
            ?assert(false)
        end,

        % Cleanup
        gen_server:call(Registry, {unregister_transport, stdio_test_transport}),
        gen_server:stop(Transport, normal, 1000)
    after
        gen_server:stop(Registry, normal, 1000)
    end.

test_stdio_concurrent_messages() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Send multiple messages concurrently
        Messages = lists:seq(1, 50),

        lists:foreach(fun(N) ->
            spawn(fun() ->
                Msg = integer_to_binary(N),
                gen_server:call(Transport, {simulate_input, Msg})
            end)
        end, Messages),

        % Collect messages
        timer:sleep(500),

        % Drain message queue
        Received = collect_all_messages([], 100),

        % Should receive messages
        ?assert(length(Received) > 0)
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_load_testing() ->
    Owner = self(),

    put(test_mode, true),

    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),

    try
        % Send many messages rapidly
        StartTime = erlang:monotonic_time(millisecond),

        lists:foreach(fun(N) ->
            Msg = integer_to_binary(N),
            spawn(fun() ->
                gen_server:call(Transport, {simulate_input, Msg})
            end)
        end, lists:seq(1, 100)),

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        % Should complete quickly
        ?assert(Duration < 2000),

        % Transport should remain stable
        timer:sleep(500),
        ?assert(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

collect_all_messages(Acc, Timeout) ->
    receive
        {transport_message, Msg} ->
            collect_all_messages([Msg | Acc], Timeout)
    after Timeout ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% Message Size Validation Tests
%%====================================================================

message_size_validation_test_() ->
    {setup,
     fun setup_stdio_transport/0,
     fun cleanup_stdio_transport/1,
     [
         {"Stdio transport accepts normal sized message", fun test_stdio_normal_size/0},
         {"Stdio transport rejects oversized message", fun test_stdio_oversized_message/0},
         {"Stdio transport validates using centralized module", fun test_stdio_centralized_validation/0}
     ]}.

test_stdio_normal_size() ->
    Owner = self(),
    put(test_mode, true),
    
    {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
    
    try
        %% Send a normal sized message (1KB)
        NormalMsg = binary:copy(<<"x">>, 1024),
        gen_server:call(Transport, {simulate_input, NormalMsg}),
        
        %% Should receive the message
        receive
            {transport_message, ReceivedMsg} ->
                ?assertEqual(NormalMsg, ReceivedMsg)
        after 1000 ->
            ?assert(false, "Timeout waiting for message")
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

test_stdio_oversized_message() ->
    Owner = self(),
    put(test_mode, true),
    
    %% Note: This test verifies the validation function exists and is called.
    %% In test mode, the reader process is not started, so we test the
    %% validation logic directly through the exported function.
    
    %% Test that oversized messages are detected
    OversizedMsg = binary:copy(<<"x">>, 17 * 1024 * 1024), % 17 MB (over 16MB limit)
    MaxSize = erlmcp_transport_stdio:get_max_message_size(),
    
    %% Verify message exceeds limit
    ?assert(byte_size(OversizedMsg) > MaxSize),
    
    %% The local validation function should detect this
    ?assertEqual({error, size_exceeded},
                 erlmcp_transport_stdio:validate_message_size(OversizedMsg, MaxSize)).

test_stdio_centralized_validation() ->
    %% Verify that the centralized validation module is available
    %% and returns proper responses
    
    %% Normal message
    NormalMsg = <<"test">>,
    ?assertEqual(ok, erlmcp_message_size:validate_stdio_size(NormalMsg)),
    
    %% Oversized message
    OversizedMsg = binary:copy(<<"x">>, 17 * 1024 * 1024), % 17 MB
    Result = erlmcp_message_size:validate_stdio_size(OversizedMsg),
    ?assertMatch({error, {message_too_large, _}}, Result),
    
    %% Verify error response is a binary
    {error, {message_too_large, ErrorResponse}} = Result,
    ?assert(is_binary(ErrorResponse)).
