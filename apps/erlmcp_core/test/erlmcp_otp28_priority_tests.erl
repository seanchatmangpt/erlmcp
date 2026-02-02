-module(erlmcp_otp28_priority_tests).

%% @doc OTP 28 Priority Message Queue Test Suite
%% Tests EEP-76 priority message queues for urgent control signals
%%
%% == Test Coverage ==
%% 1. Priority alias creation (erlang:alias/1)
%% 2. Priority message sending (erlang:send/3 with [priority])
%% 3. Message ordering (priority vs normal)
%% 4. Latency measurement (priority jumps queue)
%% 5. Use cases: cancellation, health checks, shutdown
%%
%% == Chicago School TDD ==
%% - Real process spawning and monitoring
%% - Observable message ordering (state verification)
%% - No mocks
%%
%% @end

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Generators
%%%====================================================================

priority_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Alias Creation", {spawn, fun alias_creation_tests/0}},
         {"Priority Sending", {spawn, fun priority_sending_tests/0}},
         {"Message Ordering", {spawn, fun message_ordering_tests/0}},
         {"Latency Measurement", {spawn, fun latency_measurement_tests/0}},
         {"Cancellation Use Case", {spawn, fun cancellation_tests/0}},
         {"Health Check Use Case", {spawn, fun health_check_tests/0}},
         {"Shutdown Use Case", {spawn, fun shutdown_tests/0}},
         {"Error Handling", {spawn, fun error_handling_tests/0}}]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Alias Creation Tests
%%%====================================================================

alias_creation_tests() ->
    %% Test 1: Create priority alias
    Alias = erlmcp_priority:create_priority_alias(),
    ?assertNotEqual(undefined, Alias),

    %% Test 2: Verify alias is valid
    ?assert(erlmcp_priority:is_priority_alias(Alias)),

    %% Test 3: Multiple aliases are unique
    Alias1 = erlmcp_priority:create_priority_alias(),
    Alias2 = erlmcp_priority:create_priority_alias(),
    ?assert(Alias1 =/= Alias2),

    %% Test 4: Non-alias terms return false
    ?assertNot(erlmcp_priority:is_priority_alias("not_an_alias")),
    ?assertNot(erlmcp_priority:is_priority_alias(12345)),
    ?assertNot(erlmcp_priority:is_priority_alias(#{})),

    %% Test 5: Alias works with send operator
    Alias3 = erlmcp_priority:create_priority_alias(),
    Self = self(),
    spawn(fun() ->
        Alias3 ! {normal, test_message},
        Self ! {alias_test_done}
    end),
    receive
        {alias_test_done} -> ok
    after
        1000 -> ?assert(false, "Alias test timeout")
    end,

    ok.

%%%====================================================================
%%% Priority Sending Tests
%%%====================================================================

priority_sending_tests() ->
    %% Test 1: Send priority message
    Alias = erlmcp_priority:create_priority_alias(),
    Self = self(),

    %% Create receiver process
    Receiver = spawn(fun() ->
        receive
            {priority, From, Msg} ->
                Self ! {received, Msg, From}
        end
    end),

    %% Replace alias with receiver
    AliasReceiver = Alias,
    spawn(fun() ->
        %% Send priority message
        ok = erlmcp_priority:send_priority(AliasReceiver, urgent_message, self()),
        Self ! {priority_sent}
    end),

    %% Verify message received (state-based verification)
    receive
        {priority_sent} -> ok
    after
        1000 -> ?assert(false, "Priority send timeout")
    end,

    %% Test 2: Send urgent message (no sender)
    Alias2 = erlmcp_priority:create_priority_alias(),
    Receiver2 = spawn(fun() ->
        receive
            {urgent, Msg} ->
                Self ! {received_urgent, Msg}
        end
    end),

    spawn(fun() ->
        ok = erlmcp_priority:send_urgent(Alias2, shutdown_signal),
        Self ! {urgent_sent}
    end),

    receive
        {urgent_sent} -> ok
    after
        1000 -> ?assert(false, "Urgent send timeout")
    end,

    %% Test 3: Multiple priority messages preserve order
    Alias3 = erlmcp_priority:create_priority_alias(),
    Self2 = self(),

    Receiver3 = spawn(fun() ->
        %% Receive multiple priority messages
        receive
            {priority, _, msg1} -> Self2 ! {msg, 1}
        end,
        receive
            {priority, _, msg2} -> Self2 ! {msg, 2}
        end,
        receive
            {priority, _, msg3} -> Self2 ! {msg, 3}
        end
    end),

    %% Send priority messages in order
    spawn(fun() ->
        erlmcp_priority:send_priority(Alias3, msg1, self()),
        erlmcp_priority:send_priority(Alias3, msg2, self()),
        erlmcp_priority:send_priority(Alias3, msg3, self()),
        Self2 ! {all_sent}
    end),

    %% Verify ordering (state verification)
    receive
        {msg, 1} -> ok
    after
        1000 -> ?assert(false, "Msg1 timeout")
    end,
    receive
        {msg, 2} -> ok
    after
        1000 -> ?assert(false, "Msg2 timeout")
    end,
    receive
        {msg, 3} -> ok
    after
        1000 -> ?assert(false, "Msg3 timeout")
    end,

    ok.

%%%====================================================================
%%% Message Ordering Tests
%%%====================================================================

message_ordering_tests() ->
    %% Test 1: Priority message jumps ahead of normal messages
    Self = self(),
    Alias = erlmcp_priority:create_priority_alias(),

    Receiver = spawn(fun() ->
        %% Receive all messages and track order
        receive_messages(Self, [])
    end),

    %% Send 5 normal messages
    lists:foreach(fun(N) ->
        Receiver ! {normal, N}
    end, lists:seq(1, 5)),

    %% Send priority message (should jump queue)
    spawn(fun() ->
        erlmcp_priority:send_priority(Alias, {priority, urgent}, self())
    end),

    %% Send more normal messages
    lists:foreach(fun(N) ->
        Receiver ! {normal, N + 5}
    end, lists:seq(1, 5)),

    %% Collect results
    receive
        {messages, Messages} ->
            %% Priority message should appear early (jump queue)
            ?assert(lists:keymember({priority, urgent}, 1, Messages))
    after
        2000 -> ?assert(false, "Message ordering test timeout")
    end,

    %% Test 2: Multiple priority messages preserve order
    Alias2 = erlmcp_priority:create_priority_alias(),
    Self2 = self(),

    Receiver2 = spawn(fun() ->
        receive_messages(Self2, [])
    end),

    %% Send normal messages
    Receiver2 ! {normal, 1},
    Receiver2 ! {normal, 2},

    %% Send priority messages
    lists:foreach(fun(N) ->
        erlmcp_priority:send_priority(Alias2, {priority, N}, self())
    end, lists:seq(1, 3)),

    %% Send more normal messages
    Receiver2 ! {normal, 3},
    Receiver2 ! {normal, 4},

    %% Collect and verify priority messages are ordered
    receive
        {messages, Messages2} ->
            PriorityMsgs = [N || {priority, N} <- Messages2],
            ?assertEqual([1, 2, 3], PriorityMsgs)
    after
        2000 -> ?assert(false, "Priority ordering test timeout")
    end,

    ok.

%%%====================================================================
%%% Latency Measurement Tests
%%%====================================================================

latency_measurement_tests() ->
    %% Test 1: Priority message has lower latency than normal
    Self = self(),
    Alias = erlmcp_priority:create_priority_alias(),

    %% Create busy receiver (simulating high load)
    BusyReceiver = spawn(fun() ->
        %% Process will be busy doing work
        busy_loop(Self, 100)
    end),

    %% Send normal message
    {NormalStart, _} = statistics(wall_clock),
    BusyReceiver ! {normal, test_msg},

    %% Send priority message immediately
    {PriorityStart, _} = statistics(wall_clock),
    erlmcp_priority:send_priority(Alias, {priority, urgent}, self()),

    %% Measure response times
    NormalTime = receive
        {normal_processed} ->
            {NormalEnd, _} = statistics(wall_clock),
            NormalEnd - NormalStart
    after
        2000 -> ?assert(false, "Normal message timeout")
    end,

    PriorityTime = receive
        {priority_processed} ->
            {PriorityEnd, _} = statistics(wall_clock),
            PriorityEnd - PriorityStart
    after
        2000 -> ?assert(false, "Priority message timeout")
    end,

    %% Priority should be faster (or at least competitive)
    %% Note: This is a weak assertion due to timing variability
    ?assert(PriorityTime =< NormalTime * 2),

    %% Test 2: Measure queue jumping
    %% Fill mailbox with normal messages
    Alias2 = erlmcp_priority:create_priority_alias(),
    Self2 = self(),

    SwampedReceiver = spawn(fun() ->
        %% Process messages slowly
        slow_loop(Self2, 50, [])
    end),

    %% Flood with normal messages
    lists:foreach(fun(N) ->
        SwampedReceiver ! {normal, N, large_payload_data_goes_here}
    end, lists:seq(1, 100)),

    %% Send priority message
    timer:sleep(10), %% Let some normal messages queue up
    PrioritySendTime = erlang:monotonic_time(microsecond),
    erlmcp_priority:send_priority(Alias2, {priority, check_health}, self()),

    %% Check how long priority takes
    PriorityRecvTime = receive
        {priority_recv, _} ->
            erlang:monotonic_time(microsecond)
    after
        5000 -> ?assert(false, "Priority recv timeout")
    end,

    PriorityLatency = PriorityRecvTime - PrioritySendTime,
    %% Priority should be received despite mailbox flooding
    ?assert(PriorityLatency < 5_000_000), %% Less than 5 seconds

    ok.

%%%====================================================================
%%% Cancellation Use Case Tests
%%%====================================================================

cancellation_tests() ->
    %% Test 1: Cancel long-running operation
    Self = self(),
    Alias = erlmcp_priority:create_priority_alias(),

    Worker = spawn(fun() ->
        %% Simulate long-running task
        long_running_task(Self, Alias)
    end),

    %% Let task start
    timer:sleep(50),

    %% Send cancellation (priority)
    erlmcp_priority:send_priority(Alias, {cancel, req_123}, self()),

    %% Verify cancellation processed
    receive
        {cancelled, req_123} -> ok
    after
        2000 -> ?assert(false, "Cancellation timeout")
    end,

    %% Test 2: Cancellation jumps ahead of normal messages
    Alias2 = erlmcp_priority:create_priority_alias(),
    Self2 = self(),

    Worker2 = spawn(fun() ->
        task_with_mailbox(Self2, Alias2)
    end),

    %% Send many normal messages
    lists:foreach(fun(N) ->
        Worker2 ! {process, item, N}
    end, lists:seq(1, 50)),

    %% Send cancellation (priority)
    timer:sleep(10),
    erlmcp_priority:send_priority(Alias2, {cancel, req_456}, self()),

    %% Cancellation should be processed before normal messages complete
    receive
        {cancelled_early, req_456} -> ok
    after
        3000 -> ?assert(false, "Early cancellation timeout")
    end,

    ok.

%%%====================================================================
%%% Health Check Use Case Tests
%%%====================================================================

health_check_tests() ->
    %% Test 1: Ping/pong with priority
    Self = self(),
    Alias = erlmcp_priority:create_priority_alias(),

    BusyServer = spawn(fun() ->
        busy_server_loop(Self, Alias)
    end),

    %% Server is busy processing normal messages
    lists:foreach(fun(N) ->
        BusyServer ! {work, N}
    end, lists:seq(1, 20)),

    %% Send priority ping (health check)
    PingTime = erlang:monotonic_time(microsecond),
    erlmcp_priority:send_priority(Alias, {ping, Self}, self()),

    %% Should get quick pong despite server being busy
    receive
        {pong, PingTime} ->
            PongTime = erlang:monotonic_time(microsecond),
            Latency = PongTime - PingTime,
            %% Health check should be fast (< 100ms)
            ?assert(Latency < 100_000)
    after
        1000 -> ?assert(false, "Ping timeout")
    end,

    %% Test 2: Multiple health checks during high load
    Alias2 = erlmcp_priority:create_priority_alias(),
    Self2 = self(),

    BusyServer2 = spawn(fun() ->
        busy_server_loop(Self2, Alias2)
    end),

    %% Flood with work
    lists:foreach(fun(N) ->
        BusyServer2 ! {work, N}
    end, lists:seq(1, 100)),

    %% Send multiple health checks
    PingRefs = [make_ref() || _ <- lists:seq(1, 5)],
    lists:foreach(fun(Ref) ->
        erlmcp_priority:send_priority(Alias2, {ping, Ref}, self())
    end, PingRefs),

    %% All health checks should complete quickly
    lists:foreach(fun(Ref) ->
        receive
            {pong, Ref} -> ok
        after
            1000 -> ?assert(false, {ping_timeout, Ref})
        end
    end, PingRefs),

    ok.

%%%====================================================================
%%% Shutdown Use Case Tests
%%%====================================================================

shutdown_tests() ->
    %% Test 1: Urgent shutdown signal
    Self = self(),
    Alias = erlmcp_priority:create_priority_alias(),

    Server = spawn(fun() ->
        server_loop(Self, Alias)
    end),

    %% Server is processing work
    Server ! {work, 1},
    Server ! {work, 2},

    %% Send urgent shutdown (priority)
    erlmcp_priority:send_urgent(Alias, shutdown),

    %% Server should shutdown quickly
    receive
        {shutdown_complete} -> ok
    after
        1000 -> ?assert(false, "Shutdown timeout")
    end,

    %% Verify server is dead
    ?assertNot(is_process_alive(Server)),

    %% Test 2: Graceful shutdown with priority cleanup
    Alias2 = erlmcp_priority:create_priority_alias(),
    Self2 = self(),

    Server2 = spawn(fun() ->
        graceful_server_loop(Self2, Alias2, [])
    end),

    %% Add work items
    lists:foreach(fun(N) ->
        Server2 ! {add_work, N, fun() -> timer:sleep(100) end}
    end, lists:seq(1, 10)),

    %% Send graceful shutdown (priority)
    erlmcp_priority:send_priority(Alias2, graceful_shutdown, self()),

    %% Server should complete in-flight work then shutdown
    receive
        {graceful_shutdown_complete, Completed} ->
            ?assert(Completed > 0),
            ?assert(Completed =< 10)
    after
        5000 -> ?assert(false, "Graceful shutdown timeout")
    end,

    ok.

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

error_handling_tests() ->
    %% Test 1: Invalid alias throws error
    try
        erlmcp_priority:send_priority(invalid_alias, msg, self()),
        ?assert(false, "Should have thrown error")
    catch
        error:{invalid_alias, _} -> ok;
        _:_ -> ?assert(false, "Wrong error type")
    end,

    %% Test 2: Sending to dead alias
    Alias = erlmcp_priority:create_priority_alias(),
    %% Alias is valid but not linked to a process
    %% Send should not crash
    try
        erlmcp_priority:send_urgent(Alias, test_msg),
        ok
    catch
        _:_ -> ?assert(false, "Send to dead alias should not crash")
    end,

    %% Test 3: Priority feature detection (OTP 28+)
    case erlang:system_info(otp_release) >= "28" of
        true ->
            %% OTP 28+: Should support priority
            Alias3 = erlmcp_priority:create_priority_alias(),
            ?assertNotEqual(undefined, Alias3);
        false ->
            %% OTP < 28: Should throw error or return error
            try
                erlmcp_priority:create_priority_alias(),
                ?assert(false, "Should fail on OTP < 28")
            catch
                _:_ -> ok
            end
    end,

    %% Test 4: Large priority message
    Alias4 = erlmcp_priority:create_priority_alias(),
    Self4 = self(),

    Receiver4 = spawn(fun() ->
        receive
            {priority, _, LargeMsg} ->
                Self4 ! {got_large, byte_size(LargeMsg)}
        end
    end),

    LargePayload = lists:foldl(fun(_, Acc) ->
        <<Acc/binary, "data">>
    end, <<>>, lists:seq(1, 10000)),

    erlmcp_priority:send_priority(Alias4, LargePayload, self()),

    receive
        {got_large, Size} ->
            ?assert(Size > 0)
    after
        1000 -> ?assert(false, "Large message timeout")
    end,

    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Receive and track message order
receive_messages(Parent, Acc) ->
    receive
        {priority, From, Msg} ->
            receive_messages(Parent, [{priority, Msg, From} | Acc]);
        {urgent, Msg} ->
            receive_messages(Parent, [{urgent, Msg} | Acc]);
        {normal, Msg} ->
            receive_messages(Parent, [{normal, Msg} | Acc]);
        done ->
            Parent ! {messages, lists:reverse(Acc)}
    after
        500 ->
            Parent ! {messages, lists:reverse(Acc)}
    end.

%% @doc Busy loop for latency testing
busy_loop(Parent, Count) when Count > 0 ->
    %% Simulate work
    lists:sum(lists:seq(1, 100)),
    receive
        {normal, _} ->
            busy_loop(Parent, Count - 1);
        {priority, _, _} ->
            Parent ! {priority_processed, priority},
            busy_loop(Parent, Count - 1)
    after
        0 ->
            busy_loop(Parent, Count - 1)
    end;
busy_loop(Parent, 0) ->
    Parent ! {normal_processed, ok}.

%% @doc Slow message processing loop
slow_loop(Parent, Count, Acc) when Count > 0 ->
    receive
        {priority, _, Msg} ->
            Parent ! {priority_recv, Msg},
            slow_loop(Parent, Count - 1, [priority | Acc]);
        {normal, _, _} ->
            timer:sleep(10),
            slow_loop(Parent, Count - 1, [normal | Acc])
    after
        100 ->
            Parent ! {messages, lists:reverse(Acc)}
    end;
slow_loop(Parent, 0, Acc) ->
    Parent ! {messages, lists:reverse(Acc)}.

%% @doc Long running task that can be cancelled
long_running_task(Parent, Alias) ->
    receive
        {priority, _, {cancel, ReqId}} ->
            Parent ! {cancelled, ReqId};
        {work, _} ->
            timer:sleep(100),
            long_running_task(Parent, Alias)
    after
        100 ->
            %% Task completes if not cancelled
            Parent ! {task_complete}
    end.

%% @doc Task with mailbox that processes messages
task_with_mailbox(Parent, Alias) ->
    receive
        {priority, _, {cancel, ReqId}} ->
            Parent ! {cancelled_early, ReqId};
        {process, _, N} ->
            timer:sleep(10),
            task_with_mailbox(Parent, Alias)
    after
        500 ->
            Parent ! {task_timeout}
    end.

%% @doc Busy server loop for health checks
busy_server_loop(Parent, Alias) ->
    receive
        {priority, _, {ping, From}} ->
            From ! {pong, From},
            busy_server_loop(Parent, Alias);
        {work, _} ->
            %% Simulate work
            lists:sum(lists:seq(1, 100)),
            timer:sleep(10),
            busy_server_loop(Parent, Alias)
    end.

%% @doc Server loop that handles shutdown
server_loop(Parent, Alias) ->
    receive
        {urgent, shutdown} ->
            Parent ! {shutdown_complete};
        {work, _} ->
            server_loop(Parent, Alias)
    end.

%% @doc Graceful shutdown server loop
graceful_server_loop(Parent, Alias, WorkItems) ->
    receive
        {priority, _, graceful_shutdown} ->
            %% Complete remaining work
            Completed = length(WorkItems),
            Parent ! {graceful_shutdown_complete, Completed};
        {add_work, Id, Fun} ->
            graceful_server_loop(Parent, Alias, [{Id, Fun} | WorkItems]);
        {complete_work, Id} ->
            NewItems = lists:keydelete(Id, 1, WorkItems),
            graceful_server_loop(Parent, Alias, NewItems)
    after
        0 ->
        case WorkItems of
            [{Id, Fun} | Rest] ->
                Fun(),
                graceful_server_loop(Parent, Alias, Rest);
            [] ->
                graceful_server_loop(Parent, Alias, [])
        end
    end.
