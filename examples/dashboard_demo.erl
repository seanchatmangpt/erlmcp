%%%-------------------------------------------------------------------
%%% @doc TCPS Dashboard Demo Script
%%%
%%% Demonstrates the TCPS Dashboard functionality with simulated data.
%%% Run this to see the dashboard in action with realistic metrics.
%%%
%%% Usage:
%%%   rebar3 shell
%%%   c("examples/dashboard_demo.erl").
%%%   dashboard_demo:run().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(dashboard_demo).

-export([run/0, run/1, stop/0]).
-export([simulate_work_flow/0, simulate_andon/0, simulate_kaizen/0]).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Run demo with default settings (port 8080)
-spec run() -> ok.
run() ->
    run(8080).

%% @doc Run demo on specified port
-spec run(Port :: pos_integer()) -> ok.
run(Port) ->
    io:format("~n=== TCPS Dashboard Demo ===~n~n"),

    %% Start dependencies
    io:format("Starting dependencies...~n"),
    application:ensure_all_started(jsx),
    application:ensure_all_started(cowboy),

    %% Start dashboard
    io:format("Starting TCPS Dashboard on port ~p...~n", [Port]),
    {ok, _Pid} = tcps_dashboard:start_dashboard(Port),

    %% Print access information
    io:format("~n✓ Dashboard started successfully!~n"),
    io:format("~nAccess the dashboard at: http://localhost:~p/dashboard~n", [Port]),
    io:format("~nAPI Endpoints:~n"),
    io:format("  - Metrics Summary: http://localhost:~p/api/metrics/summary~n", [Port]),
    io:format("  - Health Check:    http://localhost:~p/api/health~n", [Port]),
    io:format("  - SSE Stream:      http://localhost:~p/api/stream~n", [Port]),

    %% Start simulation
    io:format("~nStarting metric simulations...~n"),
    spawn(fun() -> simulate_metrics_loop() end),

    io:format("~nDemo is running. Try these commands:~n"),
    io:format("  - dashboard_demo:simulate_work_flow()  %% Simulate work items moving through Kanban~n"),
    io:format("  - dashboard_demo:simulate_andon()      %% Trigger an Andon alert~n"),
    io:format("  - dashboard_demo:simulate_kaizen()     %% Record Kaizen improvement~n"),
    io:format("  - dashboard_demo:stop()                %% Stop the demo~n"),
    io:format("~n"),

    ok.

%% @doc Stop the demo
-spec stop() -> ok.
stop() ->
    io:format("Stopping TCPS Dashboard demo...~n"),
    tcps_dashboard:stop_dashboard(),
    io:format("Demo stopped.~n"),
    ok.

%% @doc Simulate work flowing through Kanban buckets
-spec simulate_work_flow() -> ok.
simulate_work_flow() ->
    io:format("Simulating work flow through Kanban...~n"),

    %% Create work order
    SkuId = generate_sku_id(),
    io:format("  - Creating work order for ~s~n", [SkuId]),

    EventData = #{
        id => generate_work_order_id(),
        sku_id => SkuId,
        bucket => backlog,
        priority => random_priority()
    },

    tcps_dashboard:notify_event(work_order_completed, EventData),

    %% Simulate movement through buckets
    Buckets = [backlog, ready, in_progress, review, done],
    lists:foreach(fun(Bucket) ->
        timer:sleep(2000),
        io:format("  - Moving ~s to ~s~n", [SkuId, Bucket]),
        tcps_dashboard:notify_event(work_order_completed, EventData#{bucket => Bucket})
    end, Buckets),

    %% SKU published
    timer:sleep(1000),
    io:format("  - Publishing ~s~n", [SkuId]),
    tcps_dashboard:notify_event(sku_published, #{
        sku_id => SkuId,
        lead_time_hours => 40 + rand:uniform(20),
        quality_score => 0.90 + (rand:uniform() * 0.1)
    }),

    io:format("Work flow simulation complete.~n"),
    ok.

%% @doc Simulate an Andon alert
-spec simulate_andon() -> ok.
simulate_andon() ->
    Severities = [critical, warning, info],
    Severity = lists:nth(rand:uniform(length(Severities)), Severities),

    Titles = [
        <<"Test failure in authentication module">>,
        <<"WIP limit exceeded in review bucket">>,
        <<"Code coverage below 80% threshold">>,
        <<"Build time exceeded 5 minute limit">>,
        <<"Security vulnerability detected">>
    ],
    Title = lists:nth(rand:uniform(length(Titles)), Titles),

    io:format("Triggering ~s Andon: ~s~n", [Severity, Title]),

    EventData = #{
        id => generate_andon_id(),
        severity => Severity,
        title => Title,
        affected_skus => [generate_sku_id()],
        triggered_at => erlang:timestamp()
    },

    tcps_dashboard:notify_event(andon_triggered, EventData),

    io:format("Andon triggered. Check dashboard Andon panel.~n"),
    ok.

%% @doc Simulate Kaizen improvement
-spec simulate_kaizen() -> ok.
simulate_kaizen() ->
    Improvements = [
        {<<"Automated code review checks">>, 0.30},
        {<<"Parallel test execution">>, 0.25},
        {<<"Optimized build pipeline">>, 0.20},
        {<<"Reduced context switching">>, 0.15},
        {<<"Standardized work procedures">>, 0.10}
    ],

    {Title, ROI} = lists:nth(rand:uniform(length(Improvements)), Improvements),

    io:format("Recording Kaizen improvement: ~s (ROI: ~.0f%)~n", [Title, ROI * 100]),

    EventData = #{
        id => generate_kaizen_id(),
        title => Title,
        estimated_roi => ROI,
        status => in_progress,
        category => reduce_waste
    },

    tcps_dashboard:notify_event(kaizen_improvement, EventData),

    io:format("Kaizen improvement recorded.~n"),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Simulate periodic metric updates
simulate_metrics_loop() ->
    timer:sleep(10000), % Wait 10 seconds

    %% Randomly trigger events
    case rand:uniform(10) of
        N when N =< 3 ->
            simulate_andon();
        N when N =< 6 ->
            simulate_kaizen();
        _ ->
            ok
    end,

    simulate_metrics_loop().

%% @private Generate random SKU ID
generate_sku_id() ->
    Year = 2024,
    Number = rand:uniform(9999),
    iolist_to_binary(io_lib:format("SKU-~4..0w-~4..0w", [Year, Number])).

%% @private Generate random work order ID
generate_work_order_id() ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("WO-~p", [Timestamp])).

%% @private Generate random Andon ID
generate_andon_id() ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("ANDON-~p", [Timestamp])).

%% @private Generate random Kaizen ID
generate_kaizen_id() ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("KAIZEN-~p", [Timestamp])).

%% @private Random priority
random_priority() ->
    Priorities = [high, medium, low],
    lists:nth(rand:uniform(length(Priorities)), Priorities).
