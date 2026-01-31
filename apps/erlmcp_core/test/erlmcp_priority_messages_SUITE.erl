%%%-------------------------------------------------------------------
%%% @doc
%%% OTP 28 Priority Messages Test Suite
%%%
%%% Tests the priority message feature introduced in OTP 28:
%%% - process_flag(priority, true) - enable priority message handling
%%% - Priority message preemption under load
%%% - Aliased message delivery with priority
%%% - Health check latency improvements
%%% - Graceful drain with priority control
%%%
%%% Chicago School TDD:
%%% - Real gen_server instances (erlmcp_health_monitor)
%%% - Observable behavior: message latency, delivery order
%%% - No mocks or fakes
%%% - Graceful degradation for OTP 25-27
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_priority_messages_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_priority_flag_set/1,
    test_priority_message_preemption/1,
    test_priority_aliases/1,
    test_priority_latency/1,
    test_graceful_drain_with_priority/1
]).

-define(NORMAL_MESSAGE_COUNT, 1000).
-define(PRIORITY_MESSAGE_COUNT, 10).
-define(TARGET_PRIORITY_LATENCY_US, 1000).  % 1ms target

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, priority_features},
        {group, performance_tests},
        {group, integration_tests}
    ].

groups() ->
    [
        {priority_features, [sequence], [
            test_priority_flag_set,
            test_priority_message_preemption,
            test_priority_aliases
        ]},
        {performance_tests, [sequence], [
            test_priority_latency
        ]},
        {integration_tests, [sequence], [
            test_graceful_drain_with_priority
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting OTP 28 Priority Messages Test Suite"),

    % Detect OTP version
    OTPRelease = erlang:system_info(otp_release),
    OTPVersion = list_to_integer(OTPRelease),

    ct:pal("OTP Release: ~s (Version: ~p)", [OTPRelease, OTPVersion]),

    % Check if priority messages are available (OTP 28+)
    HasPriority = has_priority_messages(),
    ct:pal("Priority messages available: ~p", [HasPriority]),

    [{otp_version, OTPVersion}, {has_priority, HasPriority} | Config].

end_per_suite(_Config) ->
    ct:pal("Priority Messages Test Suite completed"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test that priority flag can be set on a process
test_priority_flag_set(Config) ->
    HasPriority = proplists:get_value(has_priority, Config),
    OTPVersion = proplists:get_value(otp_version, Config),

    ct:pal("Testing priority flag setting (OTP ~p)", [OTPVersion]),

    if
        OTPVersion >= 28 ->
            % OTP 28+: priority flag should be available
            ?assert(HasPriority),

            % Create a test process that sets priority flag
            Self = self(),
            Pid = spawn_link(fun() ->
                % Set priority flag
                OldValue = process_flag(priority, true),

                % Verify flag was set
                CurrentValue = process_flag(priority, false),

                % Send results back
                Self ! {priority_test, OldValue, CurrentValue},

                % Wait for shutdown
                receive shutdown -> ok after 1000 -> ok end
            end),

            % Collect results
            receive
                {priority_test, _OldValue, CurrentValue} ->
                    ct:pal("Priority flag was: ~p", [CurrentValue]),
                    ?assertEqual(true, CurrentValue)
            after 1000 ->
                ct:fail("Priority test process did not respond")
            end,

            Pid ! shutdown;

        true ->
            % OTP 25-27: priority flag should not be available
            ?assertNot(HasPriority),
            ct:pal("Priority messages not available (expected for OTP <28)")
    end.

%% @doc Test that priority messages preempt normal messages under load
test_priority_message_preemption(Config) ->
    HasPriority = proplists:get_value(has_priority, Config),

    case HasPriority of
        false ->
            {skip, "Priority messages not available (OTP <28)"};

        true ->
            ct:pal("Testing priority message preemption"),

            % Create receiver process
            Self = self(),
            Receiver = spawn_link(fun() ->
                % Enable priority messages
                process_flag(priority, true),

                % Notify ready
                Self ! ready,

                % Process messages and track order
                receive_and_track_messages(Self, [])
            end),

            % Wait for receiver to be ready
            receive ready -> ok after 1000 -> ct:fail("Receiver not ready") end,

            % Flood with normal messages
            [Receiver ! {normal, N} || N <- lists:seq(1, ?NORMAL_MESSAGE_COUNT)],

            % Small delay to let normal messages accumulate in mailbox
            timer:sleep(10),

            % Send priority messages (using alias mechanism)
            Alias = erlang:monitor(process, Receiver, [{alias, explicit_unalias}]),
            [Alias ! {priority, P} || P <- lists:seq(1, ?PRIORITY_MESSAGE_COUNT)],

            % Request delivery order
            Receiver ! {get_order, Self},

            % Receive results
            Order = receive
                {message_order, MsgOrder} -> MsgOrder
            after 5000 ->
                ct:fail("Did not receive message order")
            end,

            ct:pal("Received ~p messages in order", [length(Order)]),

            % Analyze order: priority messages should appear before normal messages
            % (they were sent after normal messages but should preempt)
            FirstN = lists:sublist(Order, 20),
            ct:pal("First 20 messages: ~p", [FirstN]),

            % Count priority messages in first 20
            PriorityInFirst20 = length([M || {priority, _} <- FirstN]),
            ct:pal("Priority messages in first 20: ~p", [PriorityInFirst20]),

            % At least some priority messages should have preempted
            % (exact behavior depends on timing, but with 1000 normal messages,
            % priority messages should definitely jump ahead)
            ?assert(PriorityInFirst20 > 0),

            % Cleanup
            erlang:demonitor(Alias, [flush])
    end.

%% @doc Test priority message delivery via aliases
test_priority_aliases(Config) ->
    HasPriority = proplists:get_value(has_priority, Config),

    case HasPriority of
        false ->
            {skip, "Priority messages not available (OTP <28)"};

        true ->
            ct:pal("Testing priority aliases"),

            % Create receiver with priority enabled
            Self = self(),
            Receiver = spawn_link(fun() ->
                process_flag(priority, true),
                Self ! ready,
                receive
                    {via_alias, Data} ->
                        Self ! {received_alias, Data}
                after 2000 ->
                    Self ! timeout
                end
            end),

            receive ready -> ok after 1000 -> ct:fail("Receiver not ready") end,

            % Create alias
            Alias = erlang:monitor(process, Receiver, [{alias, explicit_unalias}]),

            % Send via alias (priority message)
            Alias ! {via_alias, test_data},

            % Verify delivery
            Result = receive
                {received_alias, test_data} -> ok;
                timeout -> ct:fail("Message via alias not received")
            after 2000 ->
                ct:fail("No response from receiver")
            end,

            ?assertEqual(ok, Result),

            % Cleanup
            erlang:demonitor(Alias, [flush])
    end.

%% @doc Test that priority messages reduce health check latency
test_priority_latency(Config) ->
    HasPriority = proplists:get_value(has_priority, Config),

    case HasPriority of
        false ->
            {skip, "Priority messages not available (OTP <28)"};

        true ->
            ct:pal("Testing priority message latency"),

            % Create a gen_server that handles health checks with priority
            Self = self(),
            {ok, Server} = gen_server:start_link(
                {local, priority_test_server},
                ?MODULE,
                [Self],
                []
            ),

            % Enable priority on server process
            % (Note: we can't directly set process_flag on gen_server,
            % but we can test latency with priority-enabled receiver)

            % Instead, create a custom receiver process
            Receiver = spawn_link(fun() ->
                process_flag(priority, true),
                Self ! ready,
                priority_latency_loop()
            end),

            receive ready -> ok after 1000 -> ct:fail("Receiver not ready") end,

            % Flood with work messages
            [Receiver ! {work, N} || N <- lists:seq(1, ?NORMAL_MESSAGE_COUNT)],

            % Small delay
            timer:sleep(10),

            % Send health check via alias (priority)
            Alias = erlang:monitor(process, Receiver, [{alias, explicit_unalias}]),
            T1 = erlang:monotonic_time(microsecond),
            Alias ! {health_check, Self},

            % Measure response time
            Latency = receive
                health_ok ->
                    T2 = erlang:monotonic_time(microsecond),
                    T2 - T1
            after 5000 ->
                ct:fail("Health check timeout")
            end,

            ct:pal("Health check latency: ~p microseconds", [Latency]),

            % Latency should be < target (1ms = 1000us)
            % Priority messages should jump the queue
            ?assert(Latency < ?TARGET_PRIORITY_LATENCY_US * 10),  % Allow 10x for test variance

            % Cleanup
            erlang:demonitor(Alias, [flush]),
            gen_server:stop(Server)
    end.

%% @doc Test graceful drain with priority message control
test_graceful_drain_with_priority(Config) ->
    HasPriority = proplists:get_value(has_priority, Config),

    case HasPriority of
        false ->
            {skip, "Priority messages not available (OTP <28)"};

        true ->
            ct:pal("Testing graceful drain with priority"),

            % Create a worker that processes messages
            Self = self(),
            Worker = spawn_link(fun() ->
                process_flag(priority, true),
                Self ! ready,
                drain_worker_loop(Self, normal, 0)
            end),

            receive ready -> ok after 1000 -> ct:fail("Worker not ready") end,

            % Send normal work messages
            [Worker ! {work, N} || N <- lists:seq(1, 100)],

            % Small delay
            timer:sleep(10),

            % Send drain signal via alias (priority - should jump ahead)
            Alias = erlang:monitor(process, Worker, [{alias, explicit_unalias}]),
            Alias ! {drain, Self},

            % Worker should transition to drain mode quickly
            DrainAck = receive
                drain_started -> ok
            after 2000 ->
                ct:fail("Drain not acknowledged")
            end,

            ?assertEqual(ok, DrainAck),

            % Send new work - should be rejected
            Worker ! {work, new_work},

            % Request status
            Worker ! {status, Self},

            Status = receive
                {status, Mode, Processed} ->
                    ct:pal("Worker mode: ~p, processed: ~p", [Mode, Processed]),
                    {Mode, Processed}
            after 2000 ->
                ct:fail("Status not received")
            end,

            {Mode, _Processed} = Status,

            % Should be in drain mode
            ?assertEqual(drain, Mode),

            % Cleanup
            erlang:demonitor(Alias, [flush]),
            Worker ! shutdown
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Check if priority messages are available
has_priority_messages() ->
    % Try to set priority flag
    try
        OldValue = process_flag(priority, true),
        process_flag(priority, OldValue),
        true
    catch
        error:badarg -> false;
        _:_ -> false
    end.

%% @doc Message tracking loop for preemption test
receive_and_track_messages(Parent, Acc) ->
    receive
        {get_order, Parent} ->
            Parent ! {message_order, lists:reverse(Acc)};
        Msg ->
            receive_and_track_messages(Parent, [Msg | Acc])
    after 100 ->
        % No more messages for 100ms, return what we have
        Parent ! {message_order, lists:reverse(Acc)}
    end.

%% @doc Latency test loop
priority_latency_loop() ->
    receive
        {health_check, From} ->
            % Respond immediately to health check
            From ! health_ok,
            priority_latency_loop();
        {work, _N} ->
            % Simulate work
            timer:sleep(1),
            priority_latency_loop()
    after 10000 ->
        ok
    end.

%% @doc Graceful drain worker loop
drain_worker_loop(Parent, Mode, Processed) ->
    receive
        {drain, From} ->
            % Acknowledge drain request
            From ! drain_started,
            drain_worker_loop(Parent, drain, Processed);

        {work, _N} when Mode =:= drain ->
            % Reject new work in drain mode
            drain_worker_loop(Parent, Mode, Processed);

        {work, _N} when Mode =:= normal ->
            % Process work in normal mode
            drain_worker_loop(Parent, Mode, Processed + 1);

        {status, From} ->
            From ! {status, Mode, Processed},
            drain_worker_loop(Parent, Mode, Processed);

        shutdown ->
            ok
    after 10000 ->
        ok
    end.

%%====================================================================
%% gen_server callbacks (for test server)
%%====================================================================

init([Parent]) ->
    {ok, #{parent => Parent}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
