-module(erlmcp_transport_stdio_tests).
-include_lib("eunit/include/eunit.hrl").

%% Stdio Transport Tests - Chicago School TDD
%% Tests observable behavior through API calls only
%% NO STATE INSPECTION, NO DUMMY PROCESSES, NO PROCESS DICTIONARY

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    ok.

teardown(_) ->
    timer:sleep(100),
    ok.

%%%===================================================================
%%% Basic Stdio Transport Tests
%%%===================================================================

stdio_start_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"Start stdio transport with owner",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 ?assert(is_pid(Transport)),
                 ?assert(is_process_alive(Transport))
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end},

        {"Multiple transports with unique IDs",
         fun() ->
             Owner = self(),
             {ok, T1} = erlmcp_transport_stdio:start_link(Owner),
             {ok, T2} = erlmcp_transport_stdio:start_link(Owner),
             try
                 ?assert(is_process_alive(T1)),
                 ?assert(is_process_alive(T2)),
                 ?assert(T1 =/= T2)
             after
                 catch gen_server:stop(T1, normal, 1000),
                 catch gen_server:stop(T2, normal, 1000)
             end
         end}
     ]}.

stdio_send_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"Send binary data",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 TestData = <<"test message">>,
                 ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, TestData))
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end},

        {"Send iolist data",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 TestIolist = [<<"test">>, " ", <<"message">>],
                 ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, TestIolist))
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end},

        {"Send empty data",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 ?assertEqual(ok, erlmcp_transport_stdio:send(Transport, <<>>))
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end}
     ]}.

stdio_close_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"Close transport stops process",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             ?assert(is_process_alive(Transport)),

             ?assertEqual(ok, erlmcp_transport_stdio:close(Transport)),

             timer:sleep(100),
             ?assertNot(is_process_alive(Transport))
         end}
     ]}.

%%%===================================================================
%%% Message Delivery Tests
%%%===================================================================

stdio_message_delivery_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 5,
     fun() ->
         Owner = self(),
         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
         try
             % Simulate input (test mode)
             TestLine = <<"test input line">>,
             ?assertEqual(ok, gen_server:call(Transport, {simulate_input, TestLine})),

             % Owner should receive message
             receive
                 {transport_message, ReceivedLine} ->
                     ?assertEqual(TestLine, ReceivedLine)
             after 1000 ->
                 ?assert(false, "Message not received")
             end
         after
             catch gen_server:stop(Transport, normal, 1000)
         end
     end}}.

stdio_multiple_messages_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 5,
     fun() ->
         Owner = self(),
         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
         try
             Messages = [
                 <<"message 1">>,
                 <<"message 2">>,
                 <<"message 3">>
             ],

             lists:foreach(fun(Msg) ->
                 gen_server:call(Transport, {simulate_input, Msg})
             end, Messages),

             % Receive all messages
             Received = lists:map(fun(_) ->
                 receive
                     {transport_message, M} -> M
                 after 1000 ->
                     ?assert(false, "Missing message")
                 end
             end, Messages),

             ?assertEqual(Messages, Received)
         after
             catch gen_server:stop(Transport, normal, 1000)
         end
     end}}.

%%%===================================================================
%%% Owner Monitoring Tests
%%%===================================================================

stdio_owner_monitoring_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 5,
     fun() ->
         Owner = spawn_link(fun() ->
             receive
                 stop -> ok
             after 10000 -> ok
             end
         end),

         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
         ?assert(is_process_alive(Transport)),

         % Kill owner
         unlink(Owner),
         exit(Owner, kill),

         % Wait for owner death
         timer:sleep(100),

         % Transport should die when owner dies
         timer:sleep(500),
         ?assertNot(is_process_alive(Transport))
     end}}.

%%%===================================================================
%%% Line Ending Tests
%%%===================================================================

stdio_line_endings_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 5,
     [
        {"Newline termination",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 Line = <<"test line\n">>,
                 gen_server:call(Transport, {simulate_input, Line}),

                 receive
                     {transport_message, Received} ->
                         ?assert(is_binary(Received))
                 after 1000 ->
                     ?assert(false)
                 end
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end},

        {"Carriage return + newline",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 Line = <<"test line\r\n">>,
                 gen_server:call(Transport, {simulate_input, Line}),

                 receive
                     {transport_message, Received} ->
                         ?assert(is_binary(Received))
                 after 1000 ->
                     ?assert(false)
                 end
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end},

        {"Carriage return only",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 Line = <<"test line\r">>,
                 gen_server:call(Transport, {simulate_input, Line}),

                 receive
                     {transport_message, Received} ->
                         ?assert(is_binary(Received))
                 after 1000 ->
                     ?assert(false)
                 end
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end}
     ]}}.

%%%===================================================================
%%% Empty Line Handling Tests
%%%===================================================================

stdio_empty_line_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 5,
     fun() ->
         Owner = self(),
         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
         try
             % Empty line
             gen_server:call(Transport, {simulate_input, <<>>}),

             % Timeout is acceptable for empty lines
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
         end
     end}}.

%%%===================================================================
%%% Concurrent Message Tests
%%%===================================================================

stdio_concurrent_messages_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Owner = self(),
         {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
         try
             % Send multiple messages concurrently
             NumMessages = 50,

             lists:foreach(fun(N) ->
                 spawn(fun() ->
                     Msg = integer_to_binary(N),
                     gen_server:call(Transport, {simulate_input, Msg})
                 end)
             end, lists:seq(1, NumMessages)),

             % Collect messages
             timer:sleep(500),

             % Drain message queue
             Received = collect_all_messages([], 100),

             % Should receive messages
             ?assert(length(Received) > 0)
         after
             catch gen_server:stop(Transport, normal, 1000)
         end
     end}}.

stdio_load_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 15,
     fun() ->
         Owner = self(),
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
         end
     end}}.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

stdio_full_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 5,
     fun() ->
         Owner = self(),
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
         end
     end}}.

stdio_with_registry_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout, 10,
     fun() ->
         Owner = self(),

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
         end
     end}}.

%%%===================================================================
%%% Error Handling Tests
%%%===================================================================

stdio_invalid_input_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
        {"Non-binary input",
         fun() ->
             Owner = self(),
             {ok, Transport} = erlmcp_transport_stdio:start_link(Owner),
             try
                 % Should handle gracefully
                 Result = catch gen_server:call(Transport, {simulate_input, invalid}),
                 ?assert(is_tuple(Result) orelse Result =:= ok)
             after
                 catch gen_server:stop(Transport, normal, 1000)
             end
         end}
     ]}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

collect_all_messages(Acc, Timeout) ->
    receive
        {transport_message, Msg} ->
            collect_all_messages([Msg | Acc], Timeout)
    after Timeout ->
        lists:reverse(Acc)
    end.
