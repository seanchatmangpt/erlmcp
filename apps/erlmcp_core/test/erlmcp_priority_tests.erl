%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_priority (OTP 28 Priority Message Queues)
%%%
%%% Chicago School TDD: Real processes, priority queues, observable behavior
%%% Tests OTP 28 EEP-76 priority message queue functionality
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_priority_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup and Cleanup
%%%===================================================================

%% @doc Setup priority queue tests
priority_setup() ->
    %% Verify OTP 28+ availability
    case erlang:system_info(otp_release) >= "28" of
        true ->
            ok;
        false ->
            {skip, "Priority queues require OTP 28+"}
    end.

priority_cleanup(_) ->
    ok.

%%%===================================================================
%%% Test Suite: Alias Creation
%%%===================================================================

priority_alias_test_() ->
    {setup,
     fun priority_setup/0,
     fun priority_cleanup/1,
     fun(_Setup) ->
        [?_test(test_create_priority_alias()),
         ?_test(test_alias_is_unique()),
         ?_test(test_is_priority_alias_valid())]
     end}.

%% @doc Test creating a priority alias
test_create_priority_alias() ->
    Alias = erlmcp_priority:create_priority_alias(),
    ?assert(erlang:is_alias(Alias)),
    %% Verify it's a priority alias
    ?assertEqual(true, erlmcp_priority:is_priority_alias(Alias)).

%% @doc Test each alias is unique
test_alias_is_unique() ->
    Alias1 = erlmcp_priority:create_priority_alias(),
    Alias2 = erlmcp_priority:create_priority_alias(),
    %% Aliases should be different
    ?assert(Alias1 =/= Alias2).

%% @doc Test is_priority_alias validation
test_is_priority_alias_valid() ->
    Alias = erlmcp_priority:create_priority_alias(),
    ?assertEqual(true, erlmcp_priority:is_priority_alias(Alias)),
    ?assertEqual(false, erlmcp_priority:is_priority_alias(some_atom)),
    ?assertEqual(false, erlmcp_priority:is_priority_alias(make_ref())).

%%%===================================================================
%%% Test Suite: Priority Messages
%%%===================================================================

priority_message_test_() ->
    {setup,
     fun priority_setup/0,
     fun priority_cleanup/1,
     fun(_Setup) ->
        [?_test(test_send_priority_message()),
         ?_test(test_priority_from_normal_process())]
     end}.

%% @doc Test sending priority message
test_send_priority_message() ->
    %% Create test process with priority alias
    Parent = self(),
    Pid = spawn(fun() ->
        Alias = erlmcp_priority:create_priority_alias(),
        Parent ! {alias_ready, Alias},
        receive
            {priority, From, {test_msg, Data}} ->
                Parent ! {priority_received, From, Data}
        after 1000 ->
            Parent ! {timeout, priority}
        end
    end),

    %% Get alias from spawned process
    Alias = receive
        {alias_ready, A} -> A
    after 1000 ->
        error(alias_timeout)
    end,

    %% Send priority message
    ok = erlmcp_priority:send_priority(Alias, {test_msg, <<"hello">>}, self()),

    %% Verify received
    ?assertEqual({priority_received, self(), <<"hello">>},
                 receive
                     {priority_received, F, D} -> {priority_received, F, D}
                 after 1000 ->
                     timeout
                 end),

    %% Cleanup
    exit(Pid, kill).

%% @doc Test priority message from normal process
test_priority_from_normal_process() ->
    %% Test that priority works regardless of sender
    Parent = self(),
    Pid = spawn(fun() ->
        Alias = erlmcp_priority:create_priority_alias(),
        Parent ! {alias_ready, Alias},
        receive
            {priority, From, {ping, Ref}} ->
                From ! {pong, Ref},
                Parent ! priority_handled
        after 1000 ->
            Parent ! {timeout, priority}
        end
    end),

    Alias = receive
        {alias_ready, A} -> A
    after 1000 ->
        error(alias_timeout)
    end,

    %% Send ping via priority
    Ref = make_ref(),
    ok = erlmcp_priority:send_priority(Alias, {ping, Ref}, self()),

    %% Verify response
    ?assertEqual({pong, Ref},
                 receive
                     {pong, R} -> {pong, R}
                 after 1000 ->
                     timeout
                 end),

    exit(Pid, kill).

%%%===================================================================
%%% Test Suite: Urgent Messages
%%%===================================================================

urgent_message_test_() ->
    {setup,
     fun priority_setup/0,
     fun priority_cleanup/1,
     fun(_Setup) ->
        [?_test(test_send_urgent_message()),
         ?_test(test_urgent_shutdown())]
     end}.

%% @doc Test sending urgent system message
test_send_urgent_message() ->
    Parent = self(),
    Pid = spawn(fun() ->
        Alias = erlmcp_priority:create_priority_alias(),
        Parent ! {alias_ready, Alias},
        receive
            {urgent, {critical, Reason}} ->
                Parent ! {urgent_received, Reason}
        after 1000 ->
            Parent ! {timeout, urgent}
        end
    end),

    Alias = receive
        {alias_ready, A} -> A
    after 1000 ->
        error(alias_timeout)
    end,

    %% Send urgent message
    ok = erlmcp_priority:send_urgent(Alias, {critical, memory_full}),

    %% Verify received
    ?assertEqual({urgent_received, memory_full},
                 receive
                     {urgent_received, R} -> {urgent_received, R}
                 after 1000 ->
                     timeout
                 end),

    exit(Pid, kill).

%% @doc Test urgent shutdown signal
test_urgent_shutdown() ->
    Parent = self(),
    Pid = spawn(fun() ->
        Alias = erlmcp_priority:create_priority_alias(),
        Parent ! {alias_ready, Alias},
        receive
            {urgent, shutdown} ->
                Parent ! shutdown_received
        after 1000 ->
            Parent ! {timeout, shutdown}
        end
    end),

    Alias = receive
        {alias_ready, A} -> A
    after 1000 ->
        error(alias_timeout)
    end,

    %% Send shutdown signal
    ok = erlmcp_priority:send_urgent(Alias, shutdown),

    ?assertEqual(shutdown_received,
                 receive
                     shutdown_received -> shutdown_received
                 after 1000 ->
                     timeout
                 end),

    exit(Pid, kill).

%%%===================================================================
%%% Test Suite: Error Handling
%%%===================================================================

error_handling_test_() ->
    {setup,
     fun priority_setup/0,
     fun priority_cleanup/1,
     fun(_Setup) ->
        [?_test(test_send_to_invalid_alias()),
         ?_test(test_graceful_degradation_otp27())]
     end}.

%% @doc Test sending to invalid alias
test_send_to_invalid_alias() ->
    %% This should fail gracefully
    ?assertError({invalid_alias, _},
                 erlmcp_priority:send_priority(make_ref(), msg, self())).

%% @doc Test graceful degradation on OTP < 28
test_graceful_degradation_otp27() ->
    %% On OTP < 28, create_priority_alias should fail gracefully
    case erlang:system_info(otp_release) >= "28" of
        false ->
            ?assertError({otp_version_unsupported, _},
                         erlmcp_priority:create_priority_alias());
        true ->
            %% Skip on OTP 28+
            ?assert(true)
    end.
