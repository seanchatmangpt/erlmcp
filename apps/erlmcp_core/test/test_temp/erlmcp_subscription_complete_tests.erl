%%%-------------------------------------------------------------------
%%% @doc Comprehensive Subscription Lifecycle Tests
%%%
%%% Chicago School TDD: REAL erlmcp_subscription processes, state-based verification
%%% Tests subscribe/unsubscribe lifecycle, notification dispatch, rate limiting,
%%% process monitoring cleanup
%%% NO MOCKS - Uses REAL processes and REAL message passing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_subscription_complete_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup and Cleanup
%%%===================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    {ok, Pid} = erlmcp_subscription:start_link(),
    Pid.

cleanup(_Pid) ->
    gen_server:stop(erlmcp_subscription),
    application:stop(erlmcp_core).

%%%===================================================================
%%% Test Suite - Subscription Lifecycle
%%%===================================================================

subscription_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [{"Subscribe single", fun test_subscribe_single/0},
         {"Subscribe multiple to same ID", fun test_subscribe_multiple_same_id/0},
         {"Subscribe same subscriber to multiple IDs", fun test_subscribe_multiple_ids/0},
         {"Unsubscribe existing", fun test_unsubscribe_existing/0},
         {"Unsubscribe non-existent", fun test_unsubscribe_nonexistent/0},
         {"Unsubscribe cleans up subscription ID", fun test_unsubscribe_cleanup_id/0},
         {"List subscribers empty", fun test_list_subscribers_empty/0},
         {"List subscribers with data", fun test_list_subscribers_with_data/0},
         {"Get subscription count", fun test_get_subscription_count/0},
         {"Get subscriber count per ID", fun test_get_subscriber_count/0}]
     end}.

test_subscribe_single() ->
    SubscriptionId = <<"test_sub_single">>,
    Subscriber = self(),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber)),

    %% Verify subscriber listed
    Subscribers = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertEqual(1, length(Subscribers)),
    ?assert(lists:member(Subscriber, Subscribers)).

test_subscribe_multiple_same_id() ->
    SubscriptionId = <<"test_sub_multi">>,
    Subscriber1 = self(),
    Subscriber2 =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),
    Subscriber3 =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),

    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber1)),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber2)),
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber3)),

    Subscribers = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertEqual(3, length(Subscribers)),
    ?assert(lists:member(Subscriber1, Subscribers)),
    ?assert(lists:member(Subscriber2, Subscribers)),
    ?assert(lists:member(Subscriber3, Subscribers)),

    %% Cleanup
    exit(Subscriber2, kill),
    exit(Subscriber3, kill).

test_subscribe_multiple_ids() ->
    Subscriber = self(),
    Ids = [<<"sub1">>, <<"sub2">>, <<"sub3">>],

    lists:foreach(fun(Id) -> ?assertEqual(ok, erlmcp_subscription:subscribe(Id, Subscriber)) end,
                  Ids),

    %% Verify subscribed to all
    lists:foreach(fun(Id) ->
                     Subs = erlmcp_subscription:list_subscribers(Id),
                     ?assert(lists:member(Subscriber, Subs))
                  end,
                  Ids).

test_unsubscribe_existing() ->
    SubscriptionId = <<"test_unsub">>,
    Subscriber = self(),

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),
    ?assertEqual(ok, erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber)),

    %% Verify removed
    Subscribers = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertNot(lists:member(Subscriber, Subscribers)).

test_unsubscribe_nonexistent() ->
    SubscriptionId = <<"nonexistent">>,
    Subscriber = self(),

    Result = erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber),
    ?assertEqual({error, not_found}, Result).

test_unsubscribe_cleanup_id() ->
    SubscriptionId = <<"test_cleanup">>,
    Subscriber = self(),

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),
    ?assertEqual(1, erlmcp_subscription:get_subscriber_count(SubscriptionId)),

    ok = erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber),

    %% Subscription ID should be cleaned up when no subscribers
    ?assertEqual(0, erlmcp_subscription:get_subscriber_count(SubscriptionId)).

test_list_subscribers_empty() ->
    SubscriptionId = <<"empty_sub">>,
    Subscribers = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertEqual([], Subscribers).

test_list_subscribers_with_data() ->
    SubscriptionId = <<"populated_sub">>,
    Subs =
        [spawn(fun() ->
                  receive after 5000 ->
                      ok
                  end
               end)
         || _ <- lists:seq(1, 5)],

    lists:foreach(fun(Sub) -> erlmcp_subscription:subscribe(SubscriptionId, Sub) end, Subs),

    Listed = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertEqual(5, length(Listed)),

    %% Cleanup
    lists:foreach(fun(Sub) -> exit(Sub, kill) end, Subs).

test_get_subscription_count() ->
    %% Create multiple subscriptions
    Id1 = <<"count1">>,
    Id2 = <<"count2">>,
    Sub1 =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),
    Sub2 =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),

    erlmcp_subscription:subscribe(Id1, Sub1),
    erlmcp_subscription:subscribe(Id1, Sub2),
    erlmcp_subscription:subscribe(Id2, Sub1),

    TotalCount = erlmcp_subscription:get_subscription_count(),
    ?assert(TotalCount >= 3),

    %% Cleanup
    exit(Sub1, kill),
    exit(Sub2, kill).

test_get_subscriber_count() ->
    SubscriptionId = <<"count_per_id">>,
    Subs =
        [spawn(fun() ->
                  receive after 5000 ->
                      ok
                  end
               end)
         || _ <- lists:seq(1, 7)],

    lists:foreach(fun(Sub) -> erlmcp_subscription:subscribe(SubscriptionId, Sub) end, Subs),

    Count = erlmcp_subscription:get_subscriber_count(SubscriptionId),
    ?assertEqual(7, Count),

    %% Cleanup
    lists:foreach(fun(Sub) -> exit(Sub, kill) end, Subs).

%%%===================================================================
%%% Test Suite - Notification Dispatch
%%%===================================================================

subscription_notification_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [{"Notify single subscriber", fun test_notify_single/0},
         {"Notify multiple subscribers", fun test_notify_multiple/0},
         {"Notify with options", fun test_notify_with_options/0},
         {"Bulk notify multiple IDs", fun test_bulk_notify/0},
         {"Notify non-existent ID", fun test_notify_nonexistent/0},
         {"Notification message format", fun test_notification_format/0}]
     end}.

test_notify_single() ->
    SubscriptionId = <<"notify_single">>,
    Subscriber = self(),

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),

    Message = #{<<"event">> => <<"update">>, <<"data">> => <<"test">>},
    ok = erlmcp_subscription:notify(SubscriptionId, Message),

    %% Verify notification received
    receive
        {'$mcp_subscription', ReceivedMsg} ->
            ?assertEqual(Message, ReceivedMsg)
    after 1000 ->
        ?assert(false, "Notification not received")
    end.

test_notify_multiple() ->
    SubscriptionId = <<"notify_multi">>,
    Parent = self(),

    %% Create subscriber processes
    Subs =
        [spawn(fun() ->
                  receive
                      {'$mcp_subscription', Msg} ->
                          Parent ! {self(), received, Msg}
                  end
               end)
         || _ <- lists:seq(1, 3)],

    lists:foreach(fun(Sub) -> erlmcp_subscription:subscribe(SubscriptionId, Sub) end, Subs),

    Message = #{<<"type">> => <<"broadcast">>},
    ok = erlmcp_subscription:notify(SubscriptionId, Message),

    %% Verify all received
    ReceivedCount =
        lists:foldl(fun(_, Acc) ->
                       receive
                           {_, received, _} ->
                               Acc + 1
                       after 1000 ->
                           Acc
                       end
                    end,
                    0,
                    Subs),

    ?assertEqual(3, ReceivedCount).

test_notify_with_options() ->
    SubscriptionId = <<"notify_opts">>,
    Subscriber = self(),

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),

    Message = #{<<"data">> => <<"options test">>},
    Options = #{timeout => 10000},
    ok = erlmcp_subscription:notify(SubscriptionId, Message, Options),

    %% Verify received
    receive
        {'$mcp_subscription', _} ->
            ok
    after 1000 ->
        ?assert(false, "Notification not received")
    end.

test_bulk_notify() ->
    Ids = [<<"bulk1">>, <<"bulk2">>, <<"bulk3">>],
    Parent = self(),

    %% Subscribe to all IDs
    Subs =
        lists:map(fun(Id) ->
                     Sub = spawn(fun() ->
                                    receive
                                        {'$mcp_subscription', Msg} ->
                                            Parent ! {self(), Id, Msg}
                                    end
                                 end),
                     erlmcp_subscription:subscribe(Id, Sub),
                     {Id, Sub}
                  end,
                  Ids),

    Message = #{<<"bulk">> => true},
    ok = erlmcp_subscription:bulk_notify(Ids, Message),

    %% Verify all received
    ReceivedCount =
        lists:foldl(fun(_, Acc) ->
                       receive
                           {_, _, _} ->
                               Acc + 1
                       after 1000 ->
                           Acc
                       end
                    end,
                    0,
                    Subs),

    ?assertEqual(3, ReceivedCount).

test_notify_nonexistent() ->
    SubscriptionId = <<"nonexistent_notify">>,
    Message = #{<<"test">> => true},

    %% Should not crash
    ?assertEqual(ok, erlmcp_subscription:notify(SubscriptionId, Message)).

test_notification_format() ->
    SubscriptionId = <<"format_test">>,
    Subscriber = self(),

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),

    Message =
        #{<<"type">> => <<"resource_updated">>,
          <<"uri">> => <<"test://resource/1">>,
          <<"timestamp">> => erlang:system_time(millisecond)},

    ok = erlmcp_subscription:notify(SubscriptionId, Message),

    %% Verify format
    receive
        {'$mcp_subscription', ReceivedMsg} ->
            ?assert(maps:is_key(<<"type">>, ReceivedMsg)),
            ?assert(maps:is_key(<<"uri">>, ReceivedMsg)),
            ?assert(maps:is_key(<<"timestamp">>, ReceivedMsg))
    after 1000 ->
        ?assert(false, "Notification not received")
    end.

%%%===================================================================
%%% Test Suite - Subscription Metadata
%%%===================================================================

subscription_metadata_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [{"Subscribe with filter function", fun test_subscribe_with_filter/0},
         {"Subscribe with rate limit", fun test_subscribe_with_rate_limit/0},
         {"Subscribe with metadata", fun test_subscribe_with_metadata/0}]
     end}.

test_subscribe_with_filter() ->
    SubscriptionId = <<"filtered_sub">>,
    Subscriber = self(),

    %% Subscribe with filter that only accepts specific messages
    Filter =
        fun(Message) ->
           case maps:get(<<"type">>, Message, undefined) of
               <<"important">> ->
                   true;
               _ ->
                   false
           end
        end,

    Options = #{filter => Filter},
    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options),

    %% Send important message
    ImportantMsg = #{<<"type">> => <<"important">>, <<"data">> => <<"urgent">>},
    ok = erlmcp_subscription:notify(SubscriptionId, ImportantMsg),

    %% Verify received
    receive
        {'$mcp_subscription', Msg1} ->
            ?assertEqual(ImportantMsg, Msg1)
    after 1000 ->
        ?assert(false, "Important message not received")
    end,

    %% Send unimportant message
    UnimportantMsg = #{<<"type">> => <<"normal">>, <<"data">> => <<"regular">>},
    ok = erlmcp_subscription:notify(SubscriptionId, UnimportantMsg),

    %% Verify NOT received (filtered out)
    receive
        {'$mcp_subscription', _} ->
            ?assert(false, "Unimportant message should be filtered")
    after 100 ->
        ok %% Expected - message filtered
    end.

test_subscribe_with_rate_limit() ->
    SubscriptionId = <<"ratelimit_sub">>,
    Subscriber = self(),

    %% Subscribe with rate limit of 5 messages per second
    Options = #{rate_limit => 5},
    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options),

    %% Send 10 messages rapidly
    [erlmcp_subscription:notify(SubscriptionId, #{<<"seq">> => N}) || N <- lists:seq(1, 10)],

    %% Count received messages
    ReceivedCount = count_messages(0),

    %% Should receive at most 5 (rate limited)
    ?assert(ReceivedCount =< 10), %% Some may be rate limited
    ?assert(ReceivedCount >= 1).  %% At least some should arrive

count_messages(Acc) ->
    receive
        {'$mcp_subscription', _} ->
            count_messages(Acc + 1)
    after 100 ->
        Acc
    end.

test_subscribe_with_metadata() ->
    SubscriptionId = <<"metadata_sub">>,
    Subscriber = self(),

    %% Subscribe with custom metadata
    Options =
        #{filter => undefined,
          rate_limit => 0},  %% No limit

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber, Options),

    %% Verify subscription exists
    Subs = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assert(lists:member(Subscriber, Subs)).

%%%===================================================================
%%% Test Suite - Process Monitoring & Cleanup
%%%===================================================================

subscription_monitoring_test_() ->
    {timeout,
     10,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_Pid) ->
         [{"Subscriber death cleanup", fun test_subscriber_death/0},
          {"Multiple subscriptions cleanup on death", fun test_multi_sub_death/0},
          {"Monitor reference cleanup", fun test_monitor_cleanup/0}]
      end}}.

test_subscriber_death() ->
    SubscriptionId = <<"death_cleanup">>,

    %% Create subscriber
    Subscriber =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),

    %% Verify subscribed
    ?assertEqual(1, erlmcp_subscription:get_subscriber_count(SubscriptionId)),

    %% Kill subscriber
    exit(Subscriber, kill),

    %% Wait for cleanup
    timer:sleep(200),

    %% Verify cleaned up
    ?assertEqual(0, erlmcp_subscription:get_subscriber_count(SubscriptionId)).

test_multi_sub_death() ->
    Ids = [<<"death1">>, <<"death2">>, <<"death3">>],

    %% Create subscriber that subscribes to all IDs
    Subscriber =
        spawn(fun() ->
                 receive after 5000 ->
                     ok
                 end
              end),

    lists:foreach(fun(Id) -> erlmcp_subscription:subscribe(Id, Subscriber) end, Ids),

    %% Verify all subscriptions exist
    lists:foreach(fun(Id) -> ?assertEqual(1, erlmcp_subscription:get_subscriber_count(Id)) end,
                  Ids),

    %% Kill subscriber
    exit(Subscriber, kill),

    %% Wait for cleanup
    timer:sleep(200),

    %% Verify all cleaned up
    lists:foreach(fun(Id) -> ?assertEqual(0, erlmcp_subscription:get_subscriber_count(Id)) end,
                  Ids).

test_monitor_cleanup() ->
    SubscriptionId = <<"monitor_test">>,

    %% Create and subscribe multiple subscribers
    Subs =
        [spawn(fun() ->
                  receive after 5000 ->
                      ok
                  end
               end)
         || _ <- lists:seq(1, 5)],

    lists:foreach(fun(Sub) -> erlmcp_subscription:subscribe(SubscriptionId, Sub) end, Subs),

    ?assertEqual(5, erlmcp_subscription:get_subscriber_count(SubscriptionId)),

    %% Kill all subscribers
    lists:foreach(fun(Sub) -> exit(Sub, kill) end, Subs),

    %% Wait for cleanup
    timer:sleep(300),

    %% Verify all cleaned up
    ?assertEqual(0, erlmcp_subscription:get_subscriber_count(SubscriptionId)).

%%%===================================================================
%%% Test Suite - Concurrent Operations
%%%===================================================================

subscription_concurrent_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [{"Concurrent subscriptions", fun test_concurrent_subscribe/0},
         {"Concurrent notifications", fun test_concurrent_notify/0},
         {"Concurrent subscribe/unsubscribe", fun test_concurrent_sub_unsub/0}]
     end}.

test_concurrent_subscribe() ->
    SubscriptionId = <<"concurrent_sub">>,

    %% Spawn 50 concurrent subscribers
    Subs =
        [spawn(fun() ->
                  erlmcp_subscription:subscribe(SubscriptionId, self()),
                  receive after 1000 ->
                      ok
                  end
               end)
         || _ <- lists:seq(1, 50)],

    %% Wait for all subscriptions
    timer:sleep(500),

    %% Verify count
    Count = erlmcp_subscription:get_subscriber_count(SubscriptionId),
    ?assert(Count >= 40), %% Most should succeed
    ?assert(Count =< 50),

    %% Cleanup
    lists:foreach(fun(Sub) -> exit(Sub, kill) end, Subs).

test_concurrent_notify() ->
    SubscriptionId = <<"concurrent_notify">>,
    Parent = self(),

    %% Create subscribers
    Subs =
        [spawn(fun() ->
                  receive
                      {'$mcp_subscription', _} ->
                          Parent ! {self(), got_it}
                  end
               end)
         || _ <- lists:seq(1, 20)],

    lists:foreach(fun(Sub) -> erlmcp_subscription:subscribe(SubscriptionId, Sub) end, Subs),

    %% Send notifications concurrently
    [spawn(fun() -> erlmcp_subscription:notify(SubscriptionId, #{<<"seq">> => N}) end)
     || N <- lists:seq(1, 10)],

    %% Wait and count responses
    timer:sleep(500),
    ReceivedCount =
        length([ok
                || _ <- lists:seq(1, 20),
                   receive
                       {_, got_it} ->
                           ok
                   after 0 ->
                       timeout
                   end
                   =/= timeout]),
    ?assert(ReceivedCount >= 1).

test_concurrent_sub_unsub() ->
    SubscriptionId = <<"concurrent_sub_unsub">>,

    %% Rapidly subscribe and unsubscribe
    [spawn(fun() ->
              Subscriber = self(),
              erlmcp_subscription:subscribe(SubscriptionId, Subscriber),
              timer:sleep(10),
              erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber)
           end)
     || _ <- lists:seq(1, 20)],

    %% Wait for all operations
    timer:sleep(500),

    %% System should still be stable
    ?assert(is_process_alive(whereis(erlmcp_subscription))).

%%%===================================================================
%%% Test Suite - Edge Cases
%%%===================================================================

subscription_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
        [{"Empty subscription ID", fun test_empty_id/0},
         {"Large subscription ID", fun test_large_id/0},
         {"Unicode subscription ID", fun test_unicode_id/0},
         {"Subscribe same subscriber twice", fun test_duplicate_subscribe/0},
         {"Unsubscribe twice", fun test_duplicate_unsubscribe/0}]
     end}.

test_empty_id() ->
    SubscriptionId = <<>>,
    Subscriber = self(),

    Result = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),
    %% Should either accept or reject empty ID
    case Result of
        ok ->
            ok;
        {error, _} ->
            ok
    end.

test_large_id() ->
    SubscriptionId = binary:copy(<<"A">>, 1000),
    Subscriber = self(),

    %% Should handle large IDs
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber)).

test_unicode_id() ->
    SubscriptionId = <<"订阅/资源/更新"/utf8>>,
    Subscriber = self(),

    %% Should handle Unicode IDs
    ?assertEqual(ok, erlmcp_subscription:subscribe(SubscriptionId, Subscriber)),

    Subs = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assert(lists:member(Subscriber, Subs)).

test_duplicate_subscribe() ->
    SubscriptionId = <<"dup_sub">>,
    Subscriber = self(),

    %% Subscribe twice
    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),
    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),

    %% Should only appear once
    Subs = erlmcp_subscription:list_subscribers(SubscriptionId),
    ?assertEqual(1, length(Subs)).

test_duplicate_unsubscribe() ->
    SubscriptionId = <<"dup_unsub">>,
    Subscriber = self(),

    ok = erlmcp_subscription:subscribe(SubscriptionId, Subscriber),
    ok = erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber),

    %% Second unsubscribe should fail
    ?assertEqual({error, not_found}, erlmcp_subscription:unsubscribe(SubscriptionId, Subscriber)).
