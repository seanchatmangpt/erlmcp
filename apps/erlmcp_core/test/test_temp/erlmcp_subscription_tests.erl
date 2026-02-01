%%%-------------------------------------------------------------------
%%% @doc Unit Tests for erlmcp_subscription
%%%
%%% Chicago School TDD: Test ALL observable behavior through ALL interfaces.
%%% Uses REAL erlmcp_subscription processes, NEVER mocked/stubbed versions.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_subscription_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

subscription_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun subscribe_single_subscriber/0,
      fun subscribe_multiple_subscribers/0,
      fun unsubscribe_existing_subscriber/0,
      fun unsubscribe_nonexistent_subscriber/0,
      fun list_subscribers_empty/0,
      fun list_subscribers_with_data/0,
      fun notify_single_subscriber/0,
      fun notify_multiple_subscribers/0,
      fun notify_with_filter/0,
      fun bulk_notify_multiple_subscriptions/0,
      fun subscriber_death_cleanup/0,
      fun get_subscription_count/0,
      fun get_subscriber_count/0,
      fun subscribe_with_metadata/0,
      fun concurrent_subscriptions/0]}.

setup() ->
    {ok, Pid} = erlmcp_subscription:start_link(),
    Pid.

cleanup(_Pid) ->
    gen_server:stop(erlmcp_subscription).

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test subscribing a single subscriber to a subscription ID
subscribe_single_subscriber() ->
    SubscriptionId = <<"test_subscription">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber)),

    % Verify subscriber is listed
    Subscribers = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertEqual(1, length(Subscribers)),
    ?assert(lists:member(Subscriber, Subscribers)),

    % Verify count
    ?assertEqual(1, erlmcp_subscription:get_subscriber_count(SubscriptionId)).

%% @doc Test subscribing multiple subscribers to the same subscription ID
subscribe_multiple_subscribers() ->
    SubscriptionId = <<"multi_subscription">>,
    Subscriber1 = self(),
    Subscriber2 =
        spawn(fun() ->
                 receive after infinity ->
                     ok
                 end
              end),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber1)),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber2)),

    % Verify both subscribers are listed
    Subscribers = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertEqual(2, length(Subscribers)),
    ?assert(lists:member(Subscriber1, Subscribers)),
    ?assert(lists:member(Subscriber2, Subscribers)),

    % Clean up spawned process
    exit(Subscriber2, kill).

%% @doc Test unsubscribing an existing subscriber
unsubscribe_existing_subscriber() ->
    SubscriptionId = <<"unsubscribe_test">>,
    Subscriber = self(),

    % Subscribe first
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber)),

    % Verify subscription exists
    ?assertEqual(1, erlmcp_subscription:get_subscriber_count(SubscriptionId)),

    % Unsubscribe
    ?assertEqual(ok, erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber)),

    % Verify subscription is removed
    ?assertEqual(0, erlmcp_subscription:get_subscriber_count(SubscriptionId)),
    ?assertEqual([], erlmcp_subscription:list_subscribers(SubscriptionId)).

%% @doc Test unsubscribing a non-existent subscriber
unsubscribe_nonexistent_subscriber() ->
    SubscriptionId = <<"nonexistent_sub">>,
    Subscriber = self(),

    % Try to unsubscribe without subscribing
    ?assertEqual({error, not_found}, erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber)).

%% @doc Test listing subscribers when subscription doesn't exist
list_subscribers_empty() ->
    SubscriptionId = <<"empty_subscription">>,

    % Non-existent subscription should return empty list
    ?assertEqual([], erlmcp_subscription:list_subscribers(SubscriptionId)).

%% @doc Test listing subscribers with multiple subscriptions
list_subscribers_with_data() ->
    SubscriptionId1 = <<"sub1">>,
    SubscriptionId2 = <<"sub2">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId1, Subscriber)),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId2, Subscriber)),

    % Verify both subscriptions
    ?assertEqual([Subscriber], erlmcp_subscription:list_subscribers(SubscriptionId1)),
    ?assertEqual([Subscriber], erlmcp_subscription:list_subscribers(SubscriptionId2)).

%% @doc Test notifying a single subscriber
notify_single_subscriber() ->
    SubscriptionId = <<"notify_single">>,
    Subscriber = self(),

    % Subscribe and register to receive messages
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber)),

    % Send notification
    TestMessage = #{test => data},
    erlmcp_subscription:notify(SubscriptionId, TestMessage),

    % Verify message received
    receive
        {'$mcp_subscription', TestMessage} ->
            ok
    after 1000 ->
        ?assert(false, "Did not receive notification message")
    end.

%% @doc Test notifying multiple subscribers
notify_multiple_subscribers() ->
    SubscriptionId = <<"notify_multi">>,

    % Create subscribers that forward messages to test process
    Parent = self(),
    Subscriber1 =
        spawn(fun() ->
                 receive
                     {'$mcp_subscription', Msg} ->
                         Parent ! {sub1, Msg}
                 end
              end),
    Subscriber2 =
        spawn(fun() ->
                 receive
                     {'$mcp_subscription', Msg} ->
                         Parent ! {sub2, Msg}
                 end
              end),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber1)),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber2)),

    % Send notification
    TestMessage = #{multi => test},
    erlmcp_subscription:notify(SubscriptionId, TestMessage),

    % Verify both subscribers received the message
    receive
        {sub1, TestMessage} ->
            ok
    after 1000 ->
        ?assert(false, "Subscriber1 did not receive notification")
    end,

    receive
        {sub2, TestMessage} ->
            ok
    after 1000 ->
        ?assert(false, "Subscriber2 did not receive notification")
    end.

%% @doc Test notification with filter function
notify_with_filter() ->
    SubscriptionId = <<"filter_test">>,

    % Create filter that only allows messages with type = test
    FilterFun = fun(Msg) -> maps:get(type, Msg, undefined) =:= test end,

    % Subscribe with filter
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, self(), #{filter => FilterFun})),

    % Send message that passes filter
    erlmcp_subscription:notify(SubscriptionId, #{type => test}),
    receive
        {'$mcp_subscription', #{type := test}} ->
            ok
    after 1000 ->
        ?assert(false, "Did not receive filtered message")
    end,

    % Send message that doesn't pass filter
    erlmcp_subscription:notify(SubscriptionId, #{type => other}),
    receive
        {'$mcp_subscription', _} ->
            ?assert(false, "Should not have received filtered out message")
    after 500 ->
        ok  % Expected - message was filtered
    end.

%% @doc Test bulk notification to multiple subscription IDs
bulk_notify_multiple_subscriptions() ->
    SubscriptionId1 = <<"bulk1">>,
    SubscriptionId2 = <<"bulk2">>,

    % Create subscribers
    Parent = self(),
    Subscriber1 =
        spawn(fun() ->
                 receive
                     {'$mcp_subscription', Msg} ->
                         Parent ! {bulk1, Msg}
                 end
              end),
    Subscriber2 =
        spawn(fun() ->
                 receive
                     {'$mcp_subscription', Msg} ->
                         Parent ! {bulk2, Msg}
                 end
              end),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId1, Subscriber1)),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId2, Subscriber2)),

    % Send bulk notification
    TestMessage = #{bulk => data},
    erlmcp_subscription:bulk_notify([SubscriptionId1, SubscriptionId2], TestMessage),

    % Verify both subscriptions received the message
    receive
        {bulk1, TestMessage} ->
            ok
    after 1000 ->
        ?assert(false, "Subscription1 did not receive bulk notification")
    end,

    receive
        {bulk2, TestMessage} ->
            ok
    after 1000 ->
        ?assert(false, "Subscription2 did not receive bulk notification")
    end.

%% @doc Test automatic cleanup when subscriber dies
subscriber_death_cleanup() ->
    SubscriptionId = <<"death_test">>,

    % Create a subscriber that will die
    Subscriber =
        spawn(fun() ->
                 receive after infinity ->
                     ok
                 end
              end),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber)),
    ?assertEqual(1, erlmcp_subscription:get_subscriber_count(SubscriptionId)),

    % Kill the subscriber
    exit(Subscriber, kill),

    % Wait for DOWN message to be processed
    timer:sleep(100),

    % Verify automatic cleanup
    ?assertEqual(0, erlmcp_subscription:get_subscriber_count(SubscriptionId)),
    ?assertEqual([], erlmcp_subscription:list_subscribers(SubscriptionId)).

%% @doc Test getting total subscription count
get_subscription_count() ->
    % Clear any existing subscriptions
    % (Note: in real test, would use fresh process)
    SubscriptionId1 = <<"count1">>,
    SubscriptionId2 = <<"count2">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId1, Subscriber)),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId2, Subscriber)),

    % Total count should be 2
    TotalCount = erlmcp_subscription:get_subscription_count(),
    ?assert(TotalCount >= 2).

%% @doc Test getting subscriber count for specific subscription
get_subscriber_count() ->
    SubscriptionId = <<"specific_count">>,

    ?assertEqual(0, erlmcp_subscription:get_subscriber_count(SubscriptionId)),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, self())),
    ?assertEqual(1, erlmcp_subscription:get_subscriber_count(SubscriptionId)).

%% @doc Test subscribing with metadata options
subscribe_with_metadata() ->
    SubscriptionId = <<"metadata_test">>,
    Subscriber = self(),

    % Subscribe with rate limit (though not fully implemented)
    ?assertEqual(ok,
                 erlmcp_subscription:subscribe(SubscriptionId,
                                               Subscriber,
                                               #{rate_limit => 100, filter => fun(_) -> true end})),

    % Verify subscription exists
    ?assertEqual(1, erlmcp_subscription:get_subscriber_count(SubscriptionId)).

%% @doc Test concurrent subscriptions from multiple processes
concurrent_subscriptions() ->
    SubscriptionId = <<"concurrent_test">>,
    Parent = self(),
    NumSubscribers = 10,

    % Spawn multiple subscribers that stay alive
    Subscribers =
        [spawn(fun() ->
                  % Subscribe
                  ok = erlmcp_subscription:subscribe(SubscriptionId, self()),
                  Parent ! {subscribed, self()},
                  % Keep process alive
                  receive after infinity ->
                      ok
                  end
               end)
         || _ <- lists:seq(1, NumSubscribers)],

    % Wait for all subscriptions to complete
    lists:foreach(fun(_) ->
                     receive
                         {subscribed, _} ->
                             ok
                     end
                  end,
                  Subscribers),

    % Verify all subscriptions
    ?assertEqual(NumSubscribers, erlmcp_subscription:get_subscriber_count(SubscriptionId)),

    % Clean up spawned processes
    lists:foreach(fun(S) -> exit(S, kill) end, Subscribers).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Flush all messages from mailbox
flush_mailbox() ->
    receive
        _ ->
            flush_mailbox()
    after 0 ->
        ok
    end.
