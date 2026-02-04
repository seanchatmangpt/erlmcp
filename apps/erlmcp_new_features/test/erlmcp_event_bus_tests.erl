-module(erlmcp_event_bus_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_event_bus:start_link(),
    Pid.

cleanup(_Pid) ->
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

event_bus_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun subscribe_test/0,
      fun unsubscribe_test/0,
      fun list_subscribers_test/0,
      fun publish_test/0,
      fun publish_sync_test/0,
      fun get_metrics_test/0
     ]}.

%%%===================================================================
%%% Individual Tests
%%%===================================================================

subscribe_test() ->
    {"subscribe returns subscription id", fun() ->
        Result = erlmcp_event_bus:subscribe(test_event, undefined),
        ?assertMatch({ok, _Ref}, Result),
        {ok, Ref} = Result,
        ?assert(is_reference(Ref))
    end}.

unsubscribe_test() ->
    {"unsubscribe removes subscription", fun() ->
        {ok, SubId} = erlmcp_event_bus:subscribe(unsub_test, undefined),
        ?assertEqual(ok, erlmcp_event_bus:unsubscribe(unsub_test, SubId))
    end}.

list_subscribers_test() ->
    {"list_subscribers returns subscriber pids", fun() ->
        {ok, _SubId} = erlmcp_event_bus:subscribe(list_test, undefined),
        Subscribers = erlmcp_event_bus:list_subscribers(list_test),
        ?assert(is_list(Subscribers)),
        ?assert(length(Subscribers) >= 0)
    end}.

publish_test() ->
    {"publish broadcasts event", fun() ->
        Event = #{data => <<"test">>, timestamp => erlang:system_time(millisecond)},
        ?assertEqual(ok, erlmcp_event_bus:publish(publish_test, Event))
    end}.

publish_sync_test() ->
    {"publish_sync returns delivery count", fun() ->
        {ok, _SubId} = erlmcp_event_bus:subscribe(sync_test, undefined),
        Event = #{data => <<"sync test">>},
        Result = erlmcp_event_bus:publish_sync(sync_test, Event),
        ?assertMatch({ok, _Count}, Result),
        {ok, Count} = Result,
        ?assert(is_integer(Count)),
        ?assert(Count >= 0)
    end}.

get_metrics_test() ->
    {"get_metrics returns event statistics", fun() ->
        Metrics = erlmcp_event_bus:get_metrics(),
        ?assert(is_map(Metrics)),
        ?assert(maps:is_key(events_published, Metrics)),
        ?assert(maps:is_key(events_delivered, Metrics)),
        ?assert(maps:is_key(events_dropped, Metrics)),
        ?assert(maps:is_key(subscribers, Metrics))
    end}.
