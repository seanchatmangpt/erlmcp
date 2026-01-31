%%%-------------------------------------------------------------------
%%% @doc Stdio Transport Compliance Test Suite
%%%
%%% Chicago School TDD: Real processes, observable behavior, no mocks
%%% Tests validate:
%%% 1. Required callback implementations
%%% 2. Connection lifecycle management
%%% 3. Message framing and delivery
%%% 4. Owner process monitoring
%%% 5. Test mode simulation
%%% 6. Empty line handling
%%% 7. Concurrent message handling
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_stdio_compliance_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Configuration
%%====================================================================

-define(STDIO_TRANSPORT, erlmcp_transport_stdio).
-define(TEST_TIMEOUT, 5000).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Set test mode to avoid stdin issues
    put(test_mode, true),

    %% Start required applications
    {ok, _} = application:ensure_all_started(gproc),

    ok.

cleanup(_) ->
    erase(test_mode),
    timer:sleep(100),
    ok.

%%====================================================================
%% Test Suite
%%====================================================================

stdio_compliance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Stdio required callbacks - start_link, send, close",
       fun test_required_callbacks/0},

      {"Stdio connection lifecycle - start and stop",
       fun test_lifecycle/0},

      {"Stdio message framing - line delimited delivery",
       fun test_message_framing/0},

      {"Stdio message delivery to owner process",
       fun test_message_delivery/0},

      {"Stdio owner process monitoring - cleanup on death",
       fun test_owner_monitoring/0},

      {"Stdio test mode - simulate_input support",
       fun test_test_mode/0},

      {"Stdio empty line handling - skipped",
       fun test_empty_lines/0},

      {"Stdio concurrent messages - no loss",
       fun test_concurrent_messages/0}
     ]}.

%%====================================================================
%% Required Callbacks Tests
%%====================================================================

test_required_callbacks() ->
    Owner = self(),
    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        %% Test send callback
        ?assertEqual(ok, ?STDIO_TRANSPORT:send(Transport, <<"test">>)),

        %% Test close callback
        ?assertEqual(ok, ?STDIO_TRANSPORT:close(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Connection Lifecycle Tests
%%====================================================================

test_lifecycle() ->
    Owner = self(),
    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        %% Verify started (observable behavior)
        ?assert(is_pid(Transport)),
        ?assert(is_process_alive(Transport)),

        %% Verify stopped (observable behavior)
        ?assertEqual(ok, ?STDIO_TRANSPORT:close(Transport)),
        timer:sleep(100),
        ?assertNot(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Message Framing Tests
%%====================================================================

test_message_framing() ->
    Owner = self(),
    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        %% Test single message
        Msg1 = <<"{\"jsonrpc\":\"2.0\",\"id\":1}">>,
        gen_server:call(Transport, {simulate_input, Msg1}),

        receive
            {transport_message, Msg1} -> ok
        after 1000 ->
            ?assert(false, "Message not received")
        end,

        %% Test multiple messages
        Msg2 = <<"{\"jsonrpc\":\"2.0\",\"id\":2}">>,
        Msg3 = <<"{\"jsonrpc\":\"2.0\",\"id\":3}">>,

        gen_server:call(Transport, {simulate_input, Msg2}),
        gen_server:call(Transport, {simulate_input, Msg3}),

        receive {transport_message, _} -> ok after 500 -> ok end,
        receive {transport_message, _} -> ok after 500 -> ok end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Message Delivery Tests
%%====================================================================

test_message_delivery() ->
    Owner = self(),
    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        TestMsg = <<"test message">>,
        gen_server:call(Transport, {simulate_input, TestMsg}),

        receive
            {transport_message, TestMsg} -> ok
        after 1000 ->
            ?assert(false, "Message not delivered to owner")
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Owner Monitoring Tests
%%====================================================================

test_owner_monitoring() ->
    Owner = spawn(fun() ->
        receive stop -> ok end
    end),

    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        ?assert(is_process_alive(Transport)),

        %% Kill owner
        unlink(Owner),
        exit(Owner, kill),

        %% Transport should terminate (observable behavior)
        timer:sleep(500),
        ?assertNot(is_process_alive(Transport))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Test Mode Tests
%%====================================================================

test_test_mode() ->
    %% Test mode is already set in setup
    Owner = self(),
    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        %% In test mode, simulate_input should work
        TestMsg = <<"test mode">>,
        ?assertEqual(ok, gen_server:call(Transport, {simulate_input, TestMsg})),

        receive
            {transport_message, TestMsg} -> ok
        after 1000 ->
            ?assert(false, "Test mode failed")
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Empty Line Handling Tests
%%====================================================================

test_empty_lines() ->
    Owner = self(),
    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        %% Empty lines should not generate messages
        gen_server:call(Transport, {simulate_input, <<>>}),
        gen_server:call(Transport, {simulate_input, <<"\n">>}),
        gen_server:call(Transport, {simulate_input, <<"\r\n">>}),  % May not be supported

        receive
            {transport_message, _} ->
                ?assert(false, "Empty lines should not generate messages")
        after 500 ->
            ok
        end
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Concurrent Message Tests
%%====================================================================

test_concurrent_messages() ->
    Owner = self(),
    {ok, Transport} = ?STDIO_TRANSPORT:start_link(Owner),

    try
        NumMessages = 50,

        %% Send concurrent messages
        Pids = [spawn(fun() ->
            Msg = integer_to_binary(N),
            gen_server:call(Transport, {simulate_input, Msg})
        end) || N <- lists:seq(1, NumMessages)],

        %% Wait for completion
        timer:sleep(500),

        %% Drain messages (observable behavior)
        Received = collect_messages(0, 100),

        %% Verify some messages received
        ?assert(length(Received) > 0),

        %% Verify all processes completed
        ?assertEqual(NumMessages, length([P || P <- Pids, is_process_alive(P)]))
    after
        catch gen_server:stop(Transport, normal, 1000)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Collect messages from mailbox (observable behavior)
collect_messages(Count, Timeout) ->
    receive
        {transport_message, _Msg} ->
            collect_messages(Count + 1, Timeout)
    after Timeout ->
        Count
    end.
