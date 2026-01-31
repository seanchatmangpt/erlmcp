%% @doc EUnit tests for erlmcp_pubsub module
%% Tests pg-based pub/sub using real gen_server and pg processes
%% Chicago School TDD: Real processes, real pg groups, state-based verification
-module(erlmcp_pubsub_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

pubsub_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Pid) ->
         [
          ?_test(subscribe_single_subscriber_test(Pid)),
          ?_test(subscribe_multiple_subscribers_test(Pid)),
          ?_test(unsubscribe_test(Pid)),
          ?_test(broadcast_single_subscriber_test(Pid)),
          ?_test(broadcast_multiple_subscribers_test(Pid)),
          ?_test(broadcast_no_subscribers_test(Pid)),
          ?_test(list_subscribers_test(Pid)),
          ?_test(multiple_topics_test(Pid)),
          ?_test(subscriber_death_cleanup_test(Pid)),
          ?_test(concurrent_subscriptions_test(Pid)),
          ?_test(high_frequency_broadcast_test(Pid))
         ]
     end}.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup() ->
    %% Start real erlmcp_pubsub gen_server
    {ok, Pid} = erlmcp_pubsub:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop pubsub server properly
    case is_process_alive(Pid) of
        true -> erlmcp_pubsub:stop();
        false -> ok
    end,
    timer:sleep(50), %% Allow cleanup
    ok.

%%====================================================================
%% Test Functions (Chicago School: verify observable behavior)
%%====================================================================

subscribe_single_subscriber_test(PubsubPid) ->
    %% Setup: Spawn subscriber process
    Subscriber = spawn_subscriber(),

    %% Exercise: Subscribe to topic
    ok = erlmcp_pubsub:subscribe(test_topic, Subscriber),

    %% Verify: Subscriber listed (state verification)
    Subscribers = erlmcp_pubsub:list_subscribers(test_topic),
    ?assert(lists:member(Subscriber, Subscribers)),

    %% Cleanup
    exit(Subscriber, kill).

subscribe_multiple_subscribers_test(PubsubPid) ->
    %% Setup: Spawn 5 subscriber processes
    Subscribers = [spawn_subscriber() || _ <- lists:seq(1, 5)],

    %% Exercise: Subscribe all to same topic
    [ok = erlmcp_pubsub:subscribe(multi_topic, Sub) || Sub <- Subscribers],

    %% Verify: All subscribers listed
    Listed = erlmcp_pubsub:list_subscribers(multi_topic),
    ?assertEqual(5, length(Listed)),
    [?assert(lists:member(Sub, Listed)) || Sub <- Subscribers],

    %% Cleanup
    [exit(Sub, kill) || Sub <- Subscribers].

unsubscribe_test(PubsubPid) ->
    %% Setup: Subscribe then unsubscribe
    Subscriber = spawn_subscriber(),
    ok = erlmcp_pubsub:subscribe(unsub_topic, Subscriber),

    %% Verify: Initially subscribed
    ?assert(lists:member(Subscriber, erlmcp_pubsub:list_subscribers(unsub_topic))),

    %% Exercise: Unsubscribe
    ok = erlmcp_pubsub:unsubscribe(unsub_topic, Subscriber),

    %% Verify: No longer listed (state verification)
    Listed = erlmcp_pubsub:list_subscribers(unsub_topic),
    ?assertNot(lists:member(Subscriber, Listed)),

    %% Cleanup
    exit(Subscriber, kill).

broadcast_single_subscriber_test(PubsubPid) ->
    %% Setup: Subscribe to topic
    Self = self(),
    Subscriber = spawn(fun() ->
        receive
            {erlmcp_pubsub, Topic, Message} ->
                Self ! {received, Topic, Message}
        after 2000 ->
            Self ! timeout
        end
    end),
    ok = erlmcp_pubsub:subscribe(broadcast_single, Subscriber),

    %% Exercise: Broadcast message
    TestMessage = {data, <<"test payload">>},
    ok = erlmcp_pubsub:broadcast(broadcast_single, TestMessage),

    %% Verify: Message received (observable behavior)
    receive
        {received, broadcast_single, TestMessage} ->
            ok;
        timeout ->
            ?assert(false)
    after 1000 ->
        ?assert(false)
    end.

broadcast_multiple_subscribers_test(PubsubPid) ->
    %% Setup: Spawn 10 subscribers
    Self = self(),
    Subscribers = [spawn(fun() ->
        receive
            {erlmcp_pubsub, _Topic, Message} ->
                Self ! {received, self(), Message}
        end
    end) || _ <- lists:seq(1, 10)],

    %% Subscribe all
    [ok = erlmcp_pubsub:subscribe(broadcast_multi, Sub) || Sub <- Subscribers],

    %% Exercise: Broadcast to all
    TestMessage = {broadcast, <<"multi-subscriber test">>},
    ok = erlmcp_pubsub:broadcast(broadcast_multi, TestMessage),

    %% Verify: All 10 subscribers received message
    Received = collect_messages(10, 2000),
    ?assertEqual(10, length(Received)),

    %% Verify: All received same message
    AllCorrect = lists:all(fun({_Pid, Msg}) -> Msg =:= TestMessage end, Received),
    ?assert(AllCorrect).

broadcast_no_subscribers_test(PubsubPid) ->
    %% Exercise: Broadcast to topic with no subscribers (should not crash)
    ok = erlmcp_pubsub:broadcast(empty_topic, {msg, <<"test">>}),

    %% Verify: pubsub server still alive
    ?assert(is_process_alive(PubsubPid)).

list_subscribers_test(PubsubPid) ->
    %% Setup: Create subscribers for different topics
    Sub1 = spawn_subscriber(),
    Sub2 = spawn_subscriber(),
    Sub3 = spawn_subscriber(),

    ok = erlmcp_pubsub:subscribe(topic_a, Sub1),
    ok = erlmcp_pubsub:subscribe(topic_a, Sub2),
    ok = erlmcp_pubsub:subscribe(topic_b, Sub3),

    %% Exercise: List subscribers for each topic
    ListA = erlmcp_pubsub:list_subscribers(topic_a),
    ListB = erlmcp_pubsub:list_subscribers(topic_b),
    ListC = erlmcp_pubsub:list_subscribers(topic_nonexistent),

    %% Verify: Correct subscribers per topic
    ?assertEqual(2, length(ListA)),
    ?assert(lists:member(Sub1, ListA)),
    ?assert(lists:member(Sub2, ListA)),

    ?assertEqual(1, length(ListB)),
    ?assert(lists:member(Sub3, ListB)),

    ?assertEqual(0, length(ListC)),

    %% Cleanup
    [exit(Sub, kill) || Sub <- [Sub1, Sub2, Sub3]].

multiple_topics_test(PubsubPid) ->
    %% Setup: One subscriber for multiple topics
    Self = self(),
    Subscriber = spawn(fun() -> multi_topic_receiver(Self, 3) end),

    ok = erlmcp_pubsub:subscribe(topic_x, Subscriber),
    ok = erlmcp_pubsub:subscribe(topic_y, Subscriber),
    ok = erlmcp_pubsub:subscribe(topic_z, Subscriber),

    %% Exercise: Broadcast to each topic
    ok = erlmcp_pubsub:broadcast(topic_x, msg_x),
    ok = erlmcp_pubsub:broadcast(topic_y, msg_y),
    ok = erlmcp_pubsub:broadcast(topic_z, msg_z),

    %% Verify: Subscriber received all 3 messages with correct topics
    Messages = collect_messages(3, 2000),
    ?assertEqual(3, length(Messages)),

    %% Verify: Each message matches expected topic
    ?assert(lists:member({Subscriber, {topic_x, msg_x}}, Messages)),
    ?assert(lists:member({Subscriber, {topic_y, msg_y}}, Messages)),
    ?assert(lists:member({Subscriber, {topic_z, msg_z}}, Messages)).

subscriber_death_cleanup_test(PubsubPid) ->
    %% Setup: Subscribe a process
    Subscriber = spawn_subscriber(),
    ok = erlmcp_pubsub:subscribe(cleanup_topic, Subscriber),

    %% Verify: Subscribed
    ?assert(lists:member(Subscriber, erlmcp_pubsub:list_subscribers(cleanup_topic))),

    %% Exercise: Kill subscriber process (real process death)
    exit(Subscriber, kill),
    timer:sleep(100), %% Allow pg cleanup

    %% Verify: Automatically removed from subscribers (pg handles cleanup)
    Listed = erlmcp_pubsub:list_subscribers(cleanup_topic),
    ?assertNot(lists:member(Subscriber, Listed)).

concurrent_subscriptions_test(PubsubPid) ->
    %% Exercise: 50 processes subscribe concurrently
    Pids = [spawn(fun() ->
        ok = erlmcp_pubsub:subscribe(concurrent_topic, self()),
        receive stop -> ok end
    end) || _ <- lists:seq(1, 50)],

    timer:sleep(200), %% Allow all subscriptions to complete

    %% Verify: All 50 processes subscribed
    Subscribers = erlmcp_pubsub:list_subscribers(concurrent_topic),
    ?assertEqual(50, length(Subscribers)),

    %% Cleanup
    [P ! stop || P <- Pids].

high_frequency_broadcast_test(PubsubPid) ->
    %% Setup: Subscribe receiver
    Self = self(),
    Subscriber = spawn(fun() -> count_messages(Self, 0, 500) end),
    ok = erlmcp_pubsub:subscribe(high_freq, Subscriber),

    %% Exercise: Broadcast 500 messages rapidly (reduced from 1000 for faster test)
    [ok = erlmcp_pubsub:broadcast(high_freq, {msg, N}) || N <- lists:seq(1, 500)],

    %% Verify: All 500 messages received
    receive
        {message_count, Count} ->
            ?assertEqual(500, Count)
    after 5000 ->
        ?assert(false) %% Timeout
    end.

%%====================================================================
%% Test Helpers (Chicago School: Real processes)
%%====================================================================

%% @doc Spawn a basic subscriber that waits for stop
spawn_subscriber() ->
    spawn(fun() ->
        receive
            stop -> ok;
            _ -> spawn_subscriber()
        end
    end).

%% @doc Collect N messages within timeout
collect_messages(N, Timeout) ->
    collect_messages(N, Timeout, []).

collect_messages(0, _Timeout, Acc) ->
    lists:reverse(Acc);
collect_messages(N, Timeout, Acc) ->
    receive
        {received, Pid, Message} ->
            collect_messages(N - 1, Timeout, [{Pid, Message} | Acc])
    after Timeout ->
        lists:reverse(Acc)
    end.

%% @doc Receiver for multiple topics test
multi_topic_receiver(Parent, 0) ->
    ok;
multi_topic_receiver(Parent, N) ->
    receive
        {erlmcp_pubsub, Topic, Message} ->
            Parent ! {received, self(), {Topic, Message}},
            multi_topic_receiver(Parent, N - 1)
    end.

%% @doc Count messages and report back
count_messages(Parent, Count, Target) ->
    if
        Count >= Target ->
            Parent ! {message_count, Count};
        true ->
            receive
                {erlmcp_pubsub, _, _} ->
                    count_messages(Parent, Count + 1, Target)
            after 1000 ->
                Parent ! {message_count, Count}
            end
    end.
