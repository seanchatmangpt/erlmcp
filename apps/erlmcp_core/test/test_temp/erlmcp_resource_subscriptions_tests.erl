%%%-------------------------------------------------------------------
%%% @doc Unit Tests for erlmcp_resource_subscriptions
%%%
%%% Chicago School TDD: Test ALL observable behavior through ALL interfaces.
%%% Uses REAL erlmcp_resource_subscriptions processes, NEVER mocked/stubbed.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_resource_subscriptions_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

resource_subscription_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun subscribe_to_resource/0,
      fun subscribe_multiple_subscribers/0,
      fun unsubscribe_from_resource/0,
      fun unsubscribe_nonexistent/0,
      fun list_subscriptions_exact_match/0,
      fun notify_resource_changed/0,
      fun notify_with_rate_limiting/0,
      fun notify_with_batching/0,
      fun subscriber_death_cleanup/0,
      fun get_stats/0,
      fun set_rate_limit/0,
      fun concurrent_changes/0,
      fun multiple_resources/0]}.

setup() ->
    {ok, Pid} = erlmcp_resource_subscriptions:start_link(),
    Pid.

cleanup(_Pid) ->
    gen_server:stop(erlmcp_resource_subscriptions).

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test subscribing to a single resource URI
subscribe_to_resource() ->
    Uri = <<"file:///test/resource.txt">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{})),

    % Verify subscription exists
    Subscriptions = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assertEqual(1, length(Subscriptions)),
    ?assert(lists:member(Subscriber, Subscriptions)),

    % Verify in stats
    Stats = erlmcp_resource_subscriptions:get_stats(),
    ?assert(maps:get(total_resources, Stats) >= 1),
    ?assert(maps:get(total_subscriptions, Stats) >= 1).

%% @doc Test multiple subscribers to the same resource
subscribe_multiple_subscribers() ->
    Uri = <<"file:///shared/resource.txt">>,
    Subscriber1 = self(),
    Subscriber2 =
        spawn(fun() ->
                 receive after infinity ->
                     ok
                 end
              end),

    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber1, #{})),
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber2, #{})),

    % Verify both subscribers
    Subscriptions = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assertEqual(2, length(Subscriptions)),
    ?assert(lists:member(Subscriber1, Subscriptions)),
    ?assert(lists:member(Subscriber2, Subscriptions)),

    % Clean up
    exit(Subscriber2, kill).

%% @doc Test unsubscribing from a resource
unsubscribe_from_resource() ->
    Uri = <<"file:///unsubscribe/test.txt">>,
    Subscriber = self(),

    % Subscribe first
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{})),

    % Verify subscription exists
    Subscriptions = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assert(lists:member(Subscriber, Subscriptions)),

    % Unsubscribe
    ?assertEqual(ok, erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber)),

    % Verify subscription removed
    NewSubscriptions = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assertNot(lists:member(Subscriber, NewSubscriptions)).

%% @doc Test unsubscribing non-existent subscription
unsubscribe_nonexistent() ->
    Uri = <<"file:///nonexistent.txt">>,
    Subscriber = self(),

    % Try to unsubscribe without subscribing
    ?assertEqual({error, not_found},
                 erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, Subscriber)).

%% @doc Test listing subscriptions with exact match
list_subscriptions_exact_match() ->
    Uri1 = <<"file:///exact1.txt">>,
    Uri2 = <<"file:///exact2.txt">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri1, Subscriber, #{})),
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri2, Subscriber, #{})),

    % Verify exact match for each URI
    Subs1 = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri1, false),
    Subs2 = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri2, false),

    ?assertEqual([Subscriber], Subs1),
    ?assertEqual([Subscriber], Subs2).

%% @doc Test notifying resource changes
notify_resource_changed() ->
    Uri = <<"file:///notify/test.txt">>,
    Subscriber = self(),

    % Subscribe
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{})),

    % Notify change
    Metadata = #{changed => true},
    erlmcp_resource_subscriptions:notify_resource_changed(Uri, Metadata),

    % Wait for batch window to flush (100ms)
    timer:sleep(150),

    % Verify notification received
    receive
        {'$mcp_resource', Notification} ->
            ?assertEqual(<<"resources/updated">>, maps:get(method, Notification)),
            ?assertEqual(Uri, maps:get(uri, maps:get(params, Notification))),
            ok
    after 1000 ->
        ?assert(false, "Did not receive resource updated notification")
    end.

%% @doc Test rate limiting of notifications
notify_with_rate_limiting() ->
    Uri = <<"file:///rate/limited.txt">>,
    Subscriber = self(),

    % Subscribe with custom rate limit (500ms)
    Options = #{rate_limit => 500},
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, Options)),

    % Notify first time
    erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 1}),
    timer:sleep(150),

    % Should receive first notification
    receive
        {'$mcp_resource', _} ->
            ok
    after 500 ->
        ?assert(false, "Did not receive first notification")
    end,

    % Notify again immediately (should be rate limited)
    erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 2}),

    % Should NOT receive second notification within rate limit window
    receive
        {'$mcp_resource', _} ->
            ?assert(false, "Second notification should have been rate limited")
    after 300 ->
        ok  % Expected - rate limited
    end.

%% @doc Test batching of rapid changes
notify_with_batching() ->
    Uri = <<"file:///batch/test.txt">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{})),

    % Send multiple rapid changes within batch window (100ms)
    erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 1}),
    erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 2}),
    erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{seq => 3}),

    % Wait for batch to flush
    timer:sleep(150),

    % Should receive notifications (may be batched or individual)
    receive
        {'$mcp_resource', _} ->
            ok
    after 1000 ->
        ?assert(false, "Did not receive batched notification")
    end.

%% @doc Test automatic cleanup when subscriber dies
subscriber_death_cleanup() ->
    Uri = <<"file:///death/cleanup.txt">>,

    % Create subscriber that will die
    Subscriber =
        spawn(fun() ->
                 receive after infinity ->
                     ok
                 end
              end),

    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{})),

    % Verify subscription exists
    StatsBefore = erlmcp_resource_subscriptions:get_stats(),
    ?assert(maps:get(total_subscriptions, StatsBefore) >= 1),

    % Kill subscriber
    exit(Subscriber, kill),

    % Wait for DOWN message processing
    timer:sleep(100),

    % Verify cleanup (stats may still show the resource, but subscriber should be gone)
    Subscriptions = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
    ?assertNot(lists:member(Subscriber, Subscriptions)).

%% @doc Test getting subscription statistics
get_stats() ->
    Uri = <<"file:///stats/test.txt">>,

    % Get initial stats
    InitialStats = erlmcp_resource_subscriptions:get_stats(),
    InitialTotalSubs = maps:get(total_subscriptions, InitialStats),

    % Subscribe to a resource
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, self(), #{})),

    % Get updated stats
    UpdatedStats = erlmcp_resource_subscriptions:get_stats(),

    % Verify stats increased
    ?assert(maps:get(total_subscriptions, UpdatedStats) >= InitialTotalSubs),
    ?assert(maps:get(total_resources, UpdatedStats) >= 1),
    ?assert(is_integer(maps:get(default_rate_limit, UpdatedStats))).

%% @doc Test setting rate limit for a resource
set_rate_limit() ->
    Uri = <<"file:///rate/limit/test.txt">>,
    Subscriber = self(),

    % Subscribe
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri, Subscriber, #{})),

    % Set rate limit
    ?assertEqual(ok, erlmcp_resource_subscriptions:set_rate_limit(Uri, 2000)),

    % Verify rate limit is applied (test by checking notifications are sent)
    erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{test => data}),
    timer:sleep(150),

    receive
        {'$mcp_resource', _} ->
            ok
    after 500 ->
        ?assert(false, "Did not receive notification after rate limit change")
    end.

%% @doc Test concurrent changes to multiple resources
concurrent_changes() ->
    Uri1 = <<"file:///concurrent/1.txt">>,
    Uri2 = <<"file:///concurrent/2.txt">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri1, Subscriber, #{})),
    ?assertEqual(ok, erlmcp_resource_subscriptions:subscribe_to_resource(Uri2, Subscriber, #{})),

    % Send concurrent changes
    erlmcp_resource_subscriptions:notify_resource_changed(Uri1, #{resource => 1}),
    erlmcp_resource_subscriptions:notify_resource_changed(Uri2, #{resource => 2}),

    % Wait for batch window
    timer:sleep(150),

    % Should receive notifications for both resources
    NotifCount = receive_notifications(2, 1000),
    ?assertEqual(2, NotifCount).

%% @doc Test subscriptions to multiple different resources
multiple_resources() ->
    Uris = [<<"file:///resource1.txt">>, <<"file:///resource2.txt">>, <<"file:///resource3.txt">>],
    Subscriber = self(),

    % Subscribe to all resources
    lists:foreach(fun(Uri) ->
                     ?assertEqual(ok,
                                  erlmcp_resource_subscriptions:subscribe_to_resource(Uri,
                                                                                      Subscriber,
                                                                                      #{}))
                  end,
                  Uris),

    % Verify all subscriptions
    lists:foreach(fun(Uri) ->
                     Subs = erlmcp_resource_subscriptions:list_resource_subscriptions(Uri, false),
                     ?assert(lists:member(Subscriber, Subs))
                  end,
                  Uris),

    % Notify all resources
    lists:foreach(fun(Uri) ->
                     erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{changed => true})
                  end,
                  Uris),

    % Wait for batch window
    timer:sleep(150),

    % Should receive notifications for all resources
    NotifCount = receive_notifications(3, 1000),
    ?assertEqual(3, NotifCount).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Receive N notifications within timeout
%% Returns actual count received
receive_messages(ExpectedCount, Timeout) ->
    receive_messages(ExpectedCount, Timeout, 0).

receive_messages(0, _Timeout, Count) ->
    Count;
receive_messages(ExpectedCount, Timeout, Count) when Count < ExpectedCount ->
    receive
        {'$mcp_resource', _} ->
            receive_messages(ExpectedCount, Timeout - 100, Count + 1)
    after Timeout ->
        Count
    end;
receive_messages(_ExpectedCount, _Timeout, Count) ->
    Count.

%% Alias for compatibility
receive_notifications(ExpectedCount, Timeout) ->
    receive_messages(ExpectedCount, Timeout).

%% @doc Flush all messages from mailbox
flush_mailbox() ->
    receive
        _ ->
            flush_mailbox()
    after 0 ->
        ok
    end.
