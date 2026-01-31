%% @doc EUnit tests for erlmcp_telemetry module
%% Tests telemetry event emission using real telemetry handlers
%% Chicago School TDD: Real processes, state-based verification
-module(erlmcp_telemetry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

telemetry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(emit_tool_call_success_test()),
          ?_test(emit_tool_call_error_test()),
          ?_test(emit_resource_read_success_test()),
          ?_test(emit_resource_read_error_test()),
          ?_test(emit_prompt_render_success_test()),
          ?_test(emit_prompt_render_error_test()),
          ?_test(emit_subscription_add_test()),
          ?_test(attach_default_handler_test()),
          ?_test(detach_default_handler_test()),
          ?_test(multiple_events_test()),
          ?_test(concurrent_events_test())
         ]
     end}.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup() ->
    %% Start telemetry application if not already started
    application:ensure_all_started(telemetry),
    %% Detach any existing handlers to start clean
    catch erlmcp_telemetry:detach_default_handler(),
    ok.

cleanup(_) ->
    %% Detach test handlers
    catch erlmcp_telemetry:detach_default_handler(),
    catch telemetry:detach(<<"test-handler">>),
    ok.

%%====================================================================
%% Test Functions (Chicago School: verify observable behavior)
%%====================================================================

emit_tool_call_success_test() ->
    %% Setup: Attach test handler to capture events
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, tool, call], Collector),

    %% Exercise: Emit tool call success event
    ok = erlmcp_telemetry:emit_tool_call(<<"test_tool">>, 1500, ok),

    %% Verify: Event received with correct measurements and metadata
    Event = receive_event(Collector, 1000),
    ?assertMatch({[erlmcp, tool, call], #{duration_us := 1500, count := 1},
                  #{tool_name := <<"test_tool">>, status := ok, status_atom := ok}}, Event).

emit_tool_call_error_test() ->
    %% Setup: Attach test handler
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, tool, call], Collector),

    %% Exercise: Emit tool call error event
    ErrorReason = {timeout, 5000},
    ok = erlmcp_telemetry:emit_tool_call(<<"failing_tool">>, 5000, {error, ErrorReason}),

    %% Verify: Event received with error metadata
    Event = receive_event(Collector, 1000),
    ?assertMatch({[erlmcp, tool, call], #{duration_us := 5000},
                  #{tool_name := <<"failing_tool">>, status := error,
                    error_reason := ErrorReason, status_atom := error}}, Event).

emit_resource_read_success_test() ->
    %% Setup: Attach test handler
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, resource, read], Collector),

    %% Exercise: Emit resource read success event
    ok = erlmcp_telemetry:emit_resource_read(<<"resource://test">>, 800, ok),

    %% Verify: Event received
    Event = receive_event(Collector, 1000),
    ?assertMatch({[erlmcp, resource, read], #{duration_us := 800},
                  #{resource_uri := <<"resource://test">>, status := ok}}, Event).

emit_resource_read_error_test() ->
    %% Setup: Attach test handler
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, resource, read], Collector),

    %% Exercise: Emit resource read error event
    ok = erlmcp_telemetry:emit_resource_read(<<"resource://missing">>, 100, {error, not_found}),

    %% Verify: Event received with error
    Event = receive_event(Collector, 1000),
    ?assertMatch({[erlmcp, resource, read], #{duration_us := 100},
                  #{resource_uri := <<"resource://missing">>, status := error,
                    error_reason := not_found}}, Event).

emit_prompt_render_success_test() ->
    %% Setup: Attach test handler
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, prompt, render], Collector),

    %% Exercise: Emit prompt render success event
    ok = erlmcp_telemetry:emit_prompt_render(<<"greeting">>, 200, ok),

    %% Verify: Event received
    Event = receive_event(Collector, 1000),
    ?assertMatch({[erlmcp, prompt, render], #{duration_us := 200},
                  #{prompt_name := <<"greeting">>, status := ok}}, Event).

emit_prompt_render_error_test() ->
    %% Setup: Attach test handler
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, prompt, render], Collector),

    %% Exercise: Emit prompt render error event
    ok = erlmcp_telemetry:emit_prompt_render(<<"bad_prompt">>, 50, {error, invalid_template}),

    %% Verify: Event received with error
    Event = receive_event(Collector, 1000),
    ?assertMatch({[erlmcp, prompt, render], #{duration_us := 50},
                  #{prompt_name := <<"bad_prompt">>, status := error,
                    error_reason := invalid_template}}, Event).

emit_subscription_add_test() ->
    %% Setup: Attach test handler
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, subscription, add], Collector),

    %% Exercise: Emit subscription add event
    SubscriberPid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_telemetry:emit_subscription_add(<<"resource://weather">>, SubscriberPid),

    %% Verify: Event received
    Event = receive_event(Collector, 1000),
    ?assertMatch({[erlmcp, subscription, add], #{count := 1},
                  #{subscription_uri := <<"resource://weather">>,
                    subscriber_pid := SubscriberPid}}, Event),

    %% Cleanup
    exit(SubscriberPid, kill).

attach_default_handler_test() ->
    %% Exercise: Attach default handler
    ok = erlmcp_telemetry:attach_default_handler(),

    %% Verify: Attempting to attach again fails (handler already exists)
    ?assertEqual({error, already_exists}, erlmcp_telemetry:attach_default_handler()),

    %% Verify: Handler processes events (smoke test - should not crash)
    ok = erlmcp_telemetry:emit_tool_call(<<"test">>, 100, ok),

    %% Cleanup
    ok = erlmcp_telemetry:detach_default_handler().

detach_default_handler_test() ->
    %% Setup: Attach handler first
    ok = erlmcp_telemetry:attach_default_handler(),

    %% Exercise: Detach handler
    ok = erlmcp_telemetry:detach_default_handler(),

    %% Verify: Detaching again fails (handler not found)
    ?assertEqual({error, not_found}, erlmcp_telemetry:detach_default_handler()).

multiple_events_test() ->
    %% Setup: Attach handler for all event types
    Collector = spawn_event_collector(),
    Events = [
        [erlmcp, tool, call],
        [erlmcp, resource, read],
        [erlmcp, prompt, render],
        [erlmcp, subscription, add]
    ],
    telemetry:attach_many(
        <<"test-multi-handler">>,
        Events,
        fun(EventName, Measurements, Metadata, _Config) ->
            Collector ! {event, EventName, Measurements, Metadata}
        end,
        #{}
    ),

    %% Exercise: Emit multiple event types
    ok = erlmcp_telemetry:emit_tool_call(<<"tool1">>, 100, ok),
    ok = erlmcp_telemetry:emit_resource_read(<<"res1">>, 200, ok),
    ok = erlmcp_telemetry:emit_prompt_render(<<"prompt1">>, 300, ok),
    TestPid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_telemetry:emit_subscription_add(<<"sub1">>, TestPid),

    %% Verify: All 4 events received
    Events1 = collect_events(Collector, 4, 2000),
    ?assertEqual(4, length(Events1)),

    %% Cleanup
    telemetry:detach(<<"test-multi-handler">>),
    exit(TestPid, kill).

concurrent_events_test() ->
    %% Setup: Attach handler
    Collector = spawn_event_collector(),
    attach_test_handler([erlmcp, tool, call], Collector),

    %% Exercise: 100 concurrent processes emit events
    Pids = [spawn(fun() ->
        ToolName = iolist_to_binary(io_lib:format("tool_~p", [N])),
        ok = erlmcp_telemetry:emit_tool_call(ToolName, N * 10, ok)
    end) || N <- lists:seq(1, 100)],

    %% Wait for all processes to complete
    [begin
        Ref = monitor(process, P),
        receive {'DOWN', Ref, process, P, _} -> ok end
    end || P <- Pids],

    %% Verify: All 100 events received (order may vary)
    Events = collect_events(Collector, 100, 5000),
    ?assertEqual(100, length(Events)),

    %% Verify: All events are tool call events
    AllToolCalls = lists:all(fun({EventName, _, _}) ->
        EventName =:= [erlmcp, tool, call]
    end, Events),
    ?assert(AllToolCalls).

%%====================================================================
%% Test Helpers (Chicago School: Real processes)
%%====================================================================

%% @doc Spawn a process that collects telemetry events
spawn_event_collector() ->
    spawn(fun() -> event_collector_loop([]) end).

event_collector_loop(Events) ->
    receive
        {event, EventName, Measurements, Metadata} ->
            event_collector_loop([{EventName, Measurements, Metadata} | Events]);
        {get_events, From} ->
            From ! {events, lists:reverse(Events)},
            event_collector_loop(Events);
        stop ->
            ok
    end.

%% @doc Attach a test handler for a specific event
attach_test_handler(EventName, Collector) ->
    HandlerId = iolist_to_binary(io_lib:format("test-handler-~p", [erlang:unique_integer([positive])])),
    telemetry:attach(
        HandlerId,
        EventName,
        fun(Event, Measurements, Metadata, _Config) ->
            Collector ! {event, Event, Measurements, Metadata}
        end,
        #{}
    ).

%% @doc Receive a single event from collector
receive_event(Collector, Timeout) ->
    Collector ! {get_events, self()},
    receive
        {events, [Event | _]} -> Event
    after Timeout ->
        timeout
    end.

%% @doc Collect N events from collector
collect_events(Collector, N, Timeout) ->
    timer:sleep(100), %% Allow events to accumulate
    Collector ! {get_events, self()},
    receive
        {events, Events} when length(Events) >= N ->
            lists:sublist(Events, N);
        {events, Events} ->
            Events
    after Timeout ->
        []
    end.
