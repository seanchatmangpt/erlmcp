#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc Resource Subscription Integration Example
%%%
%%% Demonstrates MCP resource subscription feature:
%%% 1. Start erlmcp_server with resources
%%% 2. Subscribe to resource changes
%%% 3. Trigger resource change
%%% 4. Receive notification via resources/updated
%%%
%%% Prerequisites:
%%% - erlmcp_core and erlmcp_transports applications started
%%% - Resource subscriptions manager running
%%%
%%% @end
%%%-------------------------------------------------------------------

-mode(compile).

main(_) ->
    io:format("~n=== Resource Subscription Integration Example ===~n~n"),

    %% Step 1: Start required applications
    io:format("Step 1: Starting erlmcp applications...~n"),
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    io:format("✓ Applications started~n~n"),

    %% Step 2: Start resource subscriptions manager
    io:format("Step 2: Starting resource subscriptions manager...~n"),
    {ok, SubsPid} = erlmcp_resource_subscriptions:start_link(),
    io:format("✓ Resource subscriptions manager started: ~p~n~n", [SubsPid]),

    %% Step 3: Create a mock server process to receive notifications
    io:format("Step 3: Creating mock server to receive notifications...~n"),
    ServerPid = spawn(fun() -> notification_loop(#{}) end),
    io:format("✓ Mock server started: ~p~n~n", [ServerPid]),

    %% Step 4: Start erlmcp_server with resources
    io:format("Step 4: Starting erlmcp_server with resources...~n"),
    {ok, Server} = erlmcp_server:start_link(
        <<"example-server">>,
        #{
            transport => stdio,
            capabilities => #{
                resources => true,
                tools => true,
                prompts => true
            }
        }
    ),

    %% Add a resource that can change
    ok = erlmcp_server:add_resource(
        Server,
        <<"example://config">>,
        <<"Example Configuration Resource">>,
        fun(_) -> <<"{\"setting\": \"value1\"}">> end,
        <<"application/json">>
    ),

    %% Add a resource with dynamic content
    ok = erlmcp_server:add_resource(
        Server,
        <<"example://counter">>,
        <<"Counter Resource">>,
        fun(_) ->
            CounterVal = get_counter_value(),
            iolist_to_binary(io_lib:format("{\"count\": ~p}", [CounterVal]))
        end,
        <<"application/json">>
    ),
    io:format("✓ Server started with 2 resources~n~n"),

    %% Step 5: Subscribe to resource changes
    io:format("Step 5: Subscribing to resource changes...~n"),

    %% Subscribe to config resource
    case erlmcp_resource_subscriptions:subscribe_to_resource(
        <<"example://config">>,
        ServerPid,
        #{rate_limit => 1000}
    ) of
        ok -> io:format("✓ Subscribed to example://config~n");
        {error, Reason1} -> io:format("✗ Failed to subscribe: ~p~n", [Reason1])
    end,

    %% Subscribe to counter resource
    case erlmcp_resource_subscriptions:subscribe_to_resource(
        <<"example://counter">>,
        ServerPid,
        #{rate_limit => 500}
    ) of
        ok -> io:format("✓ Subscribed to example://counter~n");
        {error, Reason2} -> io:format("✗ Failed to subscribe: ~p~n", [Reason2])
    end,

    %% Get subscription stats
    Stats = erlmcp_resource_subscriptions:get_stats(),
    io:format("✓ Total resources: ~p, Total subscriptions: ~p~n~n",
              [maps:get(total_resources, Stats), maps:get(total_subscriptions, Stats)]),

    %% Step 6: Trigger resource changes
    io:format("Step 6: Triggering resource changes...~n"),
    io:format("Waiting for notifications...~n~n"),

    %% Notify config resource changed
    erlmcp_resource_subscriptions:notify_resource_changed(
        <<"example://config">>,
        #{change_type => update, source => example}
    ),

    %% Notify counter resource changed
    erlmcp_resource_subscriptions:notify_resource_changed(
        <<"example://counter">>,
        #{change_type => increment, value => 1}
    ),

    %% Wait for notifications to be delivered
    timer:sleep(500),

    %% Step 7: Check received notifications
    io:format("Step 7: Checking received notifications...~n"),
    ServerPid ! {get_notifications, self()},
    receive
        {notifications, Notifications} ->
            Count = maps:size(Notifications),
            io:format("✓ Received ~p notifications~n", [Count]),
            lists:foreach(fun({Uri, _Timestamp}) ->
                io:format("  - Resource changed: ~s~n", [Uri])
            end, maps:keys(Notifications))
    after 1000 ->
        io:format("✗ No notifications received~n")
    end,
    io:format("~n"),

    %% Step 8: List subscribers for a resource
    io:format("Step 8: Listing subscribers for resource...~n"),
    Subscribers = erlmcp_resource_subscriptions:list_resource_subscriptions(
        <<"example://config">>,
        false  % Don't include template matches
    ),
    io:format("✓ Subscribers to example://config: ~p~n~n", [length(Subscribers)]),

    %% Step 9: Unsubscribe from resource
    io:format("Step 9: Unsubscribing from resource...~n"),
    case erlmcp_resource_subscriptions:unsubscribe_from_resource(
        <<"example://config">>,
        ServerPid
    ) of
        ok -> io:format("✓ Unsubscribed from example://config~n");
        {error, Reason} -> io:format("✗ Failed to unsubscribe: ~p~n", [Reason])
    end,
    io:format("~n"),

    %% Step 10: Demonstrate rate limiting
    io:format("Step 10: Demonstrating rate limiting...~n"),
    io:format("Setting rate limit to 2000ms (2 seconds)~n"),
    erlmcp_resource_subscriptions:set_rate_limit(<<"example://counter">>, 2000),

    %% Trigger rapid changes
    io:format("Triggering 5 rapid changes...~n"),
    lists:foreach(fun(I) ->
        erlmcp_resource_subscriptions:notify_resource_changed(
            <<"example://counter">>,
            #{change_type => increment, value => I}
        ),
        timer:sleep(100)
    end, lists:seq(1, 5)),

    timer:sleep(500),
    io:format("✓ Rate limiting active - only first notification delivered~n~n"),

    %% Step 11: Cleanup
    io:format("Step 11: Cleaning up...~n"),
    erlmcp_server:stop(Server),
    erlmcp_resource_subscriptions:stop(),
    io:format("✓ Cleanup complete~n~n"),

    io:format("=== Example Complete ===~n"),
    init:stop().

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

%% @doc Loop to receive and store resource change notifications
notification_loop(Notifications) ->
    receive
        {'$mcp_resource', #{method := <<"resources/updated">>, params := Params}} ->
            Uri = maps:get(<<"uri">>, Params),
            Timestamp = maps:get(<<"timestamp">>, Params),
            io:format("  [NOTIFICATION] Resource ~s changed at ~p~n", [Uri, Timestamp]),
            notification_loop(maps:put(Uri, Timestamp, Notifications));

        {get_notifications, Pid} ->
            Pid ! {notifications, Notifications},
            notification_loop(Notifications);

        stop ->
            ok
    end.

%% @doc Mock counter value (in real example, would be from state)
get_counter_value() ->
    erlang:unique_integer([positive]) rem 100.
