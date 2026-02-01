%%%-------------------------------------------------------------------
%%% @doc
%%% Circuit Breaker POC Integration Example
%%%
%%% Shows how to integrate the circuit breaker POC with:
%%% - erlmcp_otel for telemetry
%%% - erlmcp_server for tool protection
%%% - erlmcp_dashboard_server for monitoring
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(circuit_breaker_integration_example).

-export([example_with_telemetry/0, example_per_tool_protection/0, example_dashboard_integration/0,
         example_failure_recovery/0]).

%%====================================================================
%% Integration Examples
%%====================================================================

%% @doc Example: Circuit breaker with telemetry integration
example_with_telemetry() ->
    io:format("~n=== Circuit Breaker + Telemetry Example ===~n~n"),

    %% Initialize OpenTelemetry
    ok =
        erlmcp_otel:init(#{service_name => <<"circuit_breaker_demo">>,
                           exporters => [console],
                           sampling => always_on}),

    %% Start circuit breaker for database tool
    {ok, DbBreaker} =
        erlmcp_circuit_breaker_poc:start_link(<<"database_query">>,
                                              #{failure_threshold => 3, timeout_ms => 5000}),

    %% Start a trace span
    SpanCtx =
        erlmcp_otel:start_span(<<"tool.execute">>,
                               #{<<"tool.name">> => <<"database_query">>,
                                 <<"tool.type">> => <<"database">>}),

    %% Execute calls through circuit breaker (telemetry auto-tracked)
    Results =
        lists:map(fun(N) ->
                     Result =
                         erlmcp_circuit_breaker_poc:call_tool(DbBreaker,
                                                              fun() -> simulate_database_call(N)
                                                              end),

                     %% Add result to trace
                     case Result of
                         {ok, _} ->
                             erlmcp_otel:add_event(SpanCtx,
                                                   <<"call.success">>,
                                                   #{<<"call_number">> => N});
                         {error, Reason} ->
                             erlmcp_otel:add_event(SpanCtx,
                                                   <<"call.failure">>,
                                                   #{<<"call_number">> => N,
                                                     <<"error">> => atom_to_binary(Reason, utf8)})
                     end,

                     Result
                  end,
                  lists:seq(1, 10)),

    %% End span
    erlmcp_otel:end_span(SpanCtx),

    %% Get stats
    Stats = erlmcp_circuit_breaker_poc:get_stats(DbBreaker),

    io:format("Results: ~p~n", [Results]),
    io:format("Final Stats: ~p~n", [Stats]),

    %% Cleanup
    erlmcp_circuit_breaker_poc:stop(DbBreaker),

    ok.

%% @doc Example: Per-tool circuit breaker protection
example_per_tool_protection() ->
    io:format("~n=== Per-Tool Protection Example ===~n~n"),

    %% Start circuit breakers for different tools
    Tools =
        [{<<"database">>, #{failure_threshold => 3, timeout_ms => 5000}},
         {<<"api_service">>, #{failure_threshold => 5, timeout_ms => 10000}},
         {<<"cache">>, #{failure_threshold => 2, timeout_ms => 2000}}],

    Breakers =
        lists:map(fun({ToolName, Config}) ->
                     {ok, Breaker} = erlmcp_circuit_breaker_poc:start_link(ToolName, Config),
                     {ToolName, Breaker}
                  end,
                  Tools),

    io:format("Started ~p circuit breakers~n", [length(Breakers)]),

    %% Simulate different failure patterns per tool
    lists:foreach(fun({ToolName, Breaker}) ->
                     io:format("~nTesting tool: ~s~n", [ToolName]),

                     %% Make some calls
                     Results =
                         [erlmcp_circuit_breaker_poc:call_tool(Breaker,
                                                               fun() ->
                                                                  simulate_tool_call(ToolName, N)
                                                               end)
                          || N <- lists:seq(1, 5)],

                     %% Show state
                     {State, _Data} = erlmcp_circuit_breaker_poc:get_state(Breaker),
                     io:format("  State: ~p~n", [State]),
                     io:format("  Results: ~p~n", [Results])
                  end,
                  Breakers),

    %% Show all states
    io:format("~nAll Circuit Breaker States:~n"),
    lists:foreach(fun({ToolName, Breaker}) ->
                     {State, Data} = erlmcp_circuit_breaker_poc:get_state(Breaker),
                     io:format("  ~s: ~p (trips: ~p)~n",
                               [ToolName, State, maps:get(trip_count, Data)])
                  end,
                  Breakers),

    %% Cleanup
    lists:foreach(fun({_Name, Breaker}) -> erlmcp_circuit_breaker_poc:stop(Breaker) end, Breakers),

    ok.

%% @doc Example: Dashboard integration
example_dashboard_integration() ->
    io:format("~n=== Dashboard Integration Example ===~n~n"),

    %% Start multiple breakers
    ToolsConfig =
        [{<<"tool_1">>, #{failure_threshold => 3}},
         {<<"tool_2">>, #{failure_threshold => 5}},
         {<<"tool_3">>, #{failure_threshold => 2}}],

    Breakers =
        [begin
             {ok, B} = erlmcp_circuit_breaker_poc:start_link(Name, Config),
             {Name, B}
         end
         || {Name, Config} <- ToolsConfig],

    %% Simulate activity
    simulate_activity(Breakers),

    %% Collect dashboard data
    DashboardData = collect_dashboard_data(Breakers),

    %% Display as dashboard would
    io:format("~n=== Circuit Breaker Dashboard ===~n~n"),
    display_dashboard(DashboardData),

    %% Cleanup
    [erlmcp_circuit_breaker_poc:stop(B) || {_, B} <- Breakers],

    ok.

%% @doc Example: Failure and recovery demonstration
example_failure_recovery() ->
    io:format("~n=== Failure Recovery Example ===~n~n"),

    %% Start breaker with short timeout for demo
    {ok, Breaker} =
        erlmcp_circuit_breaker_poc:start_link(<<"recovery_test">>,
                                              #{failure_threshold => 2,
                                                success_threshold => 2,
                                                timeout_ms => 1000}),

    io:format("Phase 1: Normal operation~n"),
    make_calls(Breaker, fun() -> {ok, success} end, 3),
    show_state(Breaker),

    io:format("~nPhase 2: Triggering failures~n"),
    make_calls(Breaker, fun() -> {error, simulated_failure} end, 3),
    show_state(Breaker),

    io:format("~nPhase 3: Circuit open - calls rejected~n"),
    make_calls(Breaker, fun() -> {ok, should_be_rejected} end, 2),
    show_state(Breaker),

    io:format("~nPhase 4: Waiting for recovery window...~n"),
    timer:sleep(1200),
    show_state(Breaker),

    io:format("~nPhase 5: Recovery - successful calls~n"),
    make_calls(Breaker, fun() -> {ok, recovery} end, 3),
    show_state(Breaker),

    io:format("~nPhase 6: Back to normal~n"),
    show_final_stats(Breaker),

    %% Cleanup
    erlmcp_circuit_breaker_poc:stop(Breaker),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Simulate database call
simulate_database_call(N) ->
    %% Simulate failure for some calls
    case N rem 4 of
        0 ->
            {error, timeout};
        _ ->
            {ok, {row, N, <<"data">>}}
    end.

%% @private Simulate tool call
simulate_tool_call(ToolName, N) ->
    %% Different failure patterns per tool
    case {ToolName, N rem 3} of
        {<<"database">>, 0} ->
            {error, connection_lost};
        {<<"api_service">>, 0} ->
            timer:sleep(50),
            {error, timeout};
        {<<"cache">>, 0} ->
            {error, cache_miss};
        _ ->
            {ok, {ToolName, N}}
    end.

%% @private Simulate activity on breakers
simulate_activity(Breakers) ->
    lists:foreach(fun({ToolName, Breaker}) ->
                     %% Random pattern of successes and failures
                     [erlmcp_circuit_breaker_poc:call_tool(Breaker,
                                                           fun() ->
                                                              case rand:uniform(10) of
                                                                  N when N > 7 ->
                                                                      {error, random_failure};
                                                                  _ ->
                                                                      {ok, ToolName}
                                                              end
                                                           end)
                      || _ <- lists:seq(1, 10)]
                  end,
                  Breakers).

%% @private Collect dashboard data
collect_dashboard_data(Breakers) ->
    lists:map(fun({ToolName, Breaker}) ->
                 Stats = erlmcp_circuit_breaker_poc:get_stats(Breaker),
                 {State, StateData} = erlmcp_circuit_breaker_poc:get_state(Breaker),

                 #{tool_name => ToolName,
                   state => State,
                   health => calculate_health(Stats),
                   stats => Stats,
                   state_data => StateData}
              end,
              Breakers).

%% @private Calculate health score
calculate_health(#{success_rate := SuccessRate, trip_count := Trips}) ->
    HealthScore = SuccessRate * 100 - Trips * 5,
    case HealthScore of
        Score when Score >= 90 ->
            excellent;
        Score when Score >= 75 ->
            good;
        Score when Score >= 50 ->
            fair;
        _ ->
            poor
    end.

%% @private Display dashboard
display_dashboard(DashboardData) ->
    io:format("┌────────────────┬──────────┬────────┬──────────┬───────────┐~n"),
    io:format("│ Tool           │ State    │ Health │ Success%% │ Trips     │~n"),
    io:format("├────────────────┼──────────┼────────┼──────────┼───────────┤~n"),

    lists:foreach(fun(#{tool_name := Name,
                        state := State,
                        health := Health,
                        stats := #{success_rate := SuccessRate, trip_count := Trips}}) ->
                     StateColor =
                         case State of
                             closed ->
                                 "✓";
                             half_open ->
                                 "~";
                             open ->
                                 "✗"
                         end,

                     io:format("│ ~-14s │ ~s ~-6s │ ~-6s │ ~-8.1f │ ~-9w │~n",
                               [Name, StateColor, State, Health, SuccessRate * 100, Trips])
                  end,
                  DashboardData),

    io:format("└────────────────┴──────────┴────────┴──────────┴───────────┘~n").

%% @private Make multiple calls
make_calls(Breaker, Fun, Count) ->
    Results = [erlmcp_circuit_breaker_poc:call_tool(Breaker, Fun) || _ <- lists:seq(1, Count)],
    io:format("  Made ~p calls: ~p~n", [Count, Results]).

%% @private Show current state
show_state(Breaker) ->
    {State, Data} = erlmcp_circuit_breaker_poc:get_state(Breaker),
    io:format("  State: ~p~n", [State]),
    io:format("  Consecutive failures: ~p~n", [maps:get(consecutive_failures, Data)]),
    io:format("  Consecutive successes: ~p~n", [maps:get(consecutive_successes, Data)]),
    io:format("  Trips: ~p~n", [maps:get(trip_count, Data)]).

%% @private Show final statistics
show_final_stats(Breaker) ->
    Stats = erlmcp_circuit_breaker_poc:get_stats(Breaker),
    io:format("~nFinal Statistics:~n"),
    io:format("  Total calls: ~p~n", [maps:get(total_calls, Stats)]),
    io:format("  Successes: ~p (~.1f%%)~n",
              [maps:get(total_successes, Stats), maps:get(success_rate, Stats) * 100]),
    io:format("  Failures: ~p (~.1f%%)~n",
              [maps:get(total_failures, Stats), maps:get(failure_rate, Stats) * 100]),
    io:format("  Rejected: ~p~n", [maps:get(total_rejected, Stats)]),
    io:format("  Trips: ~p~n", [maps:get(trip_count, Stats)]),
    io:format("  Avg recovery: ~.1f ms~n", [maps:get(avg_recovery_time_ms, Stats)]).
